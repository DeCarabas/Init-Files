#!/usr/bin/env python3
"""Convert a directory of PDF sourcebooks into a searchable text corpus.

With one argument, the corpus is built in place: extracted text lands in
an extracted/ subdirectory next to the PDFs, and INDEX.md and AGENTS.md
are written at the top of the directory so an LLM agent dropped there
knows how to navigate. Nothing references the PDFs by symlink, so the
whole directory stays portable (Dropbox does not sync symlinks across
machines). With a second argument, a standalone corpus is built in that
directory instead, with each book's source PDF symlinked (or copied
with --copy) alongside its text.

Requires poppler (pdfinfo, pdftotext) and, for books needing OCR,
ocrmypdf. PDFs lacking a usable text layer — missing entirely, or
present but garbled (mojibake, mirrored text from a bad OCR vendor) —
are OCR'd to a temporary file just long enough to extract their text;
the original is never modified and the OCR'd copy is not kept.

Chapter splitting is heuristic and often falls back to fixed page
chunks; reliable chapter mapping needs eyes on the table of contents,
so AGENTS.md directs agents to build a per-book toc.md from the TOC
page images as a one-time enrichment.
"""

import argparse
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

# Cloud-placeholder files (e.g. Dropbox online-only) block reads
# indefinitely; a timeout lets us skip them with a warning instead.
PROBE_TIMEOUT = 30

# Below this many extracted characters per sampled page, the text layer
# is treated as missing and the book is queued for OCR.
SPARSE_CHARS_PER_PAGE = 200

# A dense text layer can still be junk from a bad OCR vendor. Measured
# over non-whitespace characters (layout-mode extraction pads heavily
# with spaces, which would dilute the signal), real English sourcebook
# text is 0.98+ ASCII while garbled layers (CJK mojibake, mirrored
# text) sit near 0.85, so 0.95 separates cleanly.
GARBAGE_ASCII_RATIO = 0.95

# Chapter-heading detection is heuristic; when it finds fewer than this
# many headings, fixed-size chunking is used instead.
MIN_DETECTED_CHAPTERS = 3
FALLBACK_CHUNK_PAGES = 20

# A heading match this close to the previous one is almost always a
# table-of-contents entry or a running header, not a real chapter start.
MIN_CHAPTER_GAP_PAGES = 4

HEADING_RE = re.compile(r"^\s*chapter\s+([ivxlcdm]+|\d+)\b[:. ]?.{0,60}$", re.IGNORECASE)


def slugify(name: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-")
    return slug[:50].rstrip("-") or "untitled"


def pdf_info(pdf: pathlib.Path) -> dict | None:
    try:
        proc = subprocess.run(
            ["pdfinfo", str(pdf)],
            capture_output=True, text=True, timeout=PROBE_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return None
    if proc.returncode != 0:
        return None
    info = {}
    for line in proc.stdout.splitlines():
        key, _, value = line.partition(":")
        info[key.strip()] = value.strip()
    return info


def extract_text(pdf: pathlib.Path, first: int | None = None, last: int | None = None) -> str:
    cmd = ["pdftotext", "-layout", "-enc", "UTF-8"]
    if first is not None:
        cmd += ["-f", str(first), "-l", str(last)]
    cmd += [str(pdf), "-"]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    return proc.stdout if proc.returncode == 0 else ""


def ascii_ratio(text: str) -> float:
    chars = [c for c in text if not c.isspace()]
    if not chars:
        return 1.0
    return sum(1 for c in chars if c.isascii()) / len(chars)


def ocr_mode(pdf: pathlib.Path, page_count: int, args: argparse.Namespace) -> str | None:
    if args.no_ocr:
        return None
    if args.redo_ocr:
        return "--redo-ocr"
    samples = sorted({max(1, round(page_count * frac)) for frac in (0.1, 0.3, 0.5, 0.7, 0.9)})
    texts = [extract_text(pdf, p, p) for p in samples]
    if sum(len(t.strip()) for t in texts) / len(samples) < SPARSE_CHARS_PER_PAGE:
        return "--skip-text"
    if ascii_ratio("".join(texts)) < GARBAGE_ASCII_RATIO:
        return "--redo-ocr"
    return None


def extract_pages(pdf: pathlib.Path, page_count: int, args: argparse.Namespace) -> tuple[list[str], str]:
    mode = ocr_mode(pdf, page_count, args)
    if mode is None:
        return extract_text(pdf).split("\f"), "native"
    # --redo-ocr refuses some PDFs (signatures, odd structures); --force-ocr
    # rasterizes everything, which is a safe fallback for scanned books.
    attempts = [mode] + (["--force-ocr"] if mode == "--redo-ocr" else [])
    with tempfile.TemporaryDirectory() as tmpdir:
        for n, attempt in enumerate(attempts):
            print(f"  running ocrmypdf {attempt} (this can take a long time on big books)...")
            ocr_pdf = pathlib.Path(tmpdir) / f"ocr{n}.pdf"
            proc = subprocess.run(["ocrmypdf", attempt, str(pdf), str(ocr_pdf)])
            if proc.returncode == 0 and ocr_pdf.exists():
                return extract_text(ocr_pdf).split("\f"), f"OCR via ocrmypdf {attempt}"
    print("  WARNING: OCR failed, using the original text layer (may be sparse or garbled)")
    return extract_text(pdf).split("\f"), "native (OCR attempt failed)"


def find_chapters(pages: list[str]) -> list[tuple[int, str]]:
    chapters = []
    for idx, page in enumerate(pages):
        for line in page.splitlines():
            line = line.strip()
            if HEADING_RE.match(line):
                if chapters and idx - chapters[-1][0] < MIN_CHAPTER_GAP_PAGES:
                    break
                chapters.append((idx, line))
                break
    return chapters


def chunk_fallback(pages: list[str]) -> list[tuple[int, str]]:
    return [
        (start, f"Pages {start + 1}-{min(start + FALLBACK_CHUNK_PAGES, len(pages))}")
        for start in range(0, len(pages), FALLBACK_CHUNK_PAGES)
    ]


def render_pages(pages: list[str], start: int, end: int) -> str:
    return "\n".join(
        f"<!-- PDF page {i + 1} -->\n{pages[i].rstrip()}\n" for i in range(start, end + 1)
    )


def write_chapters(book_dir: pathlib.Path, title: str, source_label: str, pages: list[str],
                   chapters: list[tuple[int, str]]) -> list[tuple[str, str, str]]:
    chap_dir = book_dir / "chapters"
    if chap_dir.exists():
        shutil.rmtree(chap_dir)
    chap_dir.mkdir(parents=True)

    if chapters and chapters[0][0] > 0:
        chapters = [(0, "Front matter")] + chapters
    elif not chapters:
        chapters = [(0, "Full text")]

    written = []
    for num, (start, heading) in enumerate(chapters):
        end = (chapters[num + 1][0] - 1) if num + 1 < len(chapters) else len(pages) - 1
        fname = f"{num:02d}-{slugify(heading)}.md"
        page_range = f"{start + 1}-{end + 1}"
        body = (
            f"# {title} — {heading}\n\n"
            f"_PDF pages {page_range} of {source_label}. Machine-extracted text; special "
            f"glyphs such as game dice symbols do not survive extraction — read those "
            f"pages of the PDF directly when symbol-level precision matters._\n\n"
            + render_pages(pages, start, end)
        )
        (chap_dir / fname).write_text(body, encoding="utf-8")
        written.append((fname, page_range, heading))
    return written


def write_overview(book_dir: pathlib.Path, title: str, source_label: str, page_count: int,
                   text_source: str, split_mode: str,
                   chapters: list[tuple[str, str, str]]) -> None:
    lines = [
        f"# {title}",
        "",
        f"Source: {source_label}, {page_count} PDF pages. Text layer: {text_source}. "
        f"Chapter splitting: {split_mode}.",
        "",
        "| Chapter file | PDF pages | Heading |",
        "|---|---|---|",
    ]
    lines += [f"| chapters/{f} | {p} | {h} |" for f, p, h in chapters]
    (book_dir / "overview.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


def parse_overview(path: pathlib.Path) -> dict | None:
    """Recover a previous run's index entry so unchanged books are not re-extracted."""
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return None
    lines = text.splitlines()
    if not lines or not lines[0].startswith("# "):
        return None
    pages_match = re.search(r"(\d+) PDF pages", text)
    chapters = []
    for line in lines:
        m = re.match(r"\| chapters/(\S+) \| (\S+) \| (.+?) \|$", line)
        if m:
            chapters.append((m.group(1), m.group(2), m.group(3)))
    if pages_match is None or not chapters:
        return None
    return {"title": lines[0][2:], "pages": int(pages_match.group(1)), "chapters": chapters}


def cached_text_is_garbage(book_dir: pathlib.Path, cached: dict) -> bool:
    """Catch corpora built before garbage-text-layer detection existed."""
    ratios = []
    for fname, _, _ in cached["chapters"]:
        try:
            text = (book_dir / "chapters" / fname).read_text(encoding="utf-8")
        except OSError:
            return True
        ratios.append(ascii_ratio(text))
    return sum(ratios) / len(ratios) < GARBAGE_ASCII_RATIO


def write_corpus_docs(root: pathlib.Path, books_prefix: str, books: list[dict],
                      in_place: bool) -> None:
    index = ["# Corpus index", ""]
    for book in books:
        index.append(f"## {book['title']}")
        index.append("")
        index.append(f"Directory: `{books_prefix}/{book['slug']}/` — {book['pages']} PDF pages.")
        index.append("")
        index += [
            f"- `{books_prefix}/{book['slug']}/chapters/{f}` (pp. {p}) — {h}"
            for f, p, h in book["chapters"]
        ]
        index.append("")
    (root / "INDEX.md").write_text("\n".join(index), encoding="utf-8")

    if in_place:
        source_location = (
            "The original PDFs sit at the top level of this directory; each book's "
            "extracted text names its source PDF."
        )
    else:
        source_location = "Each book's directory contains its source PDF as `source.pdf`."

    agents = f"""# PDF reference corpus

This directory holds PDF sourcebooks together with machine-extracted text
generated by pdfcorpus.py. Each book lives under `{books_prefix}/<slug>/` with its
text split into `chapters/*.md` and an `overview.md` listing the chapters.
INDEX.md aggregates every book's chapter list. {source_location}

To answer a question from this corpus, start with INDEX.md to pick the
relevant book and chapter, or search the text directly with
`rg -i '<term>' {books_prefix}/*/chapters/`. Read only the matching chapter file,
not the whole book. Every chapter file carries `<!-- PDF page N -->`
markers; cite book and PDF page in answers.

The text was machine-extracted (some books via OCR), so tables and stat
blocks can be imperfect and special glyphs such as game dice symbols are
lost. When a passage looks garbled or symbol precision matters, open the
source PDF at the cited page and read the rendered page image instead.

Chapter splitting is mechanical and usually just fixed page chunks. If a
book has a `toc.md` in its directory, prefer it for navigation — it maps
the book's real chapters and sections to PDF pages. If asked to enrich
this corpus, build those: read the table-of-contents pages of each source
PDF directly (the page images, not the extracted text), determine the
offset between printed page numbers and PDF pages, and write a `toc.md`
per book listing each chapter and major section with its PDF page.
"""
    (root / "AGENTS.md").write_text(agents, encoding="utf-8")


def process_book(pdf: pathlib.Path, books_root: pathlib.Path, in_place: bool,
                 args: argparse.Namespace) -> dict | None:
    print(f"{pdf.name}")
    slug = slugify(pdf.stem)
    book_dir = books_root / slug

    if not args.force:
        cached = parse_overview(book_dir / "overview.md")
        if cached is not None:
            if cached_text_is_garbage(book_dir, cached):
                print("  existing extraction looks garbled; redoing with OCR")
            else:
                print("  skipped (already extracted; use --force to redo)")
                return {"slug": slug, **cached}

    info = pdf_info(pdf)
    if info is None or "Pages" not in info:
        print("  SKIPPED: unreadable (cloud placeholder not downloaded, or corrupt)")
        return None

    page_count = int(info["Pages"])
    title = info.get("Title") or pdf.stem
    # PDF Title metadata is often generic across a product line (e.g. just
    # "Core Rulebook"), so prefer the filename when it looks ambiguous.
    if title.lower() in {"core rulebook", "untitled", ""}:
        title = pdf.stem
    book_dir.mkdir(parents=True, exist_ok=True)

    if in_place:
        source_label = f"`{pdf.name}` (top level of this corpus)"
    else:
        source = book_dir / "source.pdf"
        if source.is_symlink() or source.exists():
            source.unlink()
        if args.copy:
            shutil.copy2(pdf, source)
        else:
            source.symlink_to(pdf.resolve())
        source_label = "`source.pdf` (this directory)"

    pages, text_source = extract_pages(pdf, page_count, args)
    if len(pages) > page_count:
        pages = pages[:page_count]

    chapters = find_chapters(pages)
    if len(chapters) >= MIN_DETECTED_CHAPTERS:
        split_mode = "heading-based"
    else:
        chapters = chunk_fallback(pages)
        split_mode = f"fixed {FALLBACK_CHUNK_PAGES}-page chunks (no chapter headings found)"

    written = write_chapters(book_dir, title, source_label, pages, chapters)
    write_overview(book_dir, title, source_label, page_count, text_source, split_mode, written)
    print(f"  {page_count} pages -> {len(written)} chapter files ({split_mode})")
    return {"title": title, "slug": slug, "pages": page_count, "chapters": written}


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Build a grep-friendly text corpus from a directory of PDFs."
    )
    parser.add_argument("input_dir", type=pathlib.Path,
                        help="directory containing source PDFs")
    parser.add_argument("output_dir", type=pathlib.Path, nargs="?",
                        help="standalone corpus directory; omit to build in place "
                             "under INPUT_DIR/extracted/")
    parser.add_argument("--copy", action="store_true",
                        help="copy source PDFs instead of symlinking (two-directory mode only)")
    parser.add_argument("--no-ocr", action="store_true",
                        help="never run ocrmypdf; extract whatever text layer exists")
    parser.add_argument("--redo-ocr", action="store_true",
                        help="re-OCR every book even if it has a text layer (slow, improves old OCR)")
    parser.add_argument("--force", action="store_true",
                        help="re-extract books already present in the corpus")
    args = parser.parse_args()

    in_place = args.output_dir is None or (
        args.output_dir.exists() and args.output_dir.resolve() == args.input_dir.resolve()
    )
    root = args.input_dir if in_place else args.output_dir
    books_prefix = "extracted" if in_place else "books"
    books_root = root / books_prefix

    for tool in ("pdfinfo", "pdftotext") + (() if args.no_ocr else ("ocrmypdf",)):
        if shutil.which(tool) is None:
            print(f"error: required tool '{tool}' not found on PATH", file=sys.stderr)
            return 1
    if not args.input_dir.is_dir():
        print(f"error: {args.input_dir} is not a directory", file=sys.stderr)
        return 1

    pdfs = sorted(p for p in args.input_dir.iterdir() if p.suffix.lower() == ".pdf")
    if not pdfs:
        print(f"error: no PDFs found in {args.input_dir}", file=sys.stderr)
        return 1

    books_root.mkdir(parents=True, exist_ok=True)
    books = []
    skipped = []
    for pdf in pdfs:
        result = process_book(pdf, books_root, in_place, args)
        if result is None:
            skipped.append(pdf.name)
        else:
            books.append(result)

    write_corpus_docs(root, books_prefix, books, in_place)
    print(f"\nDone: {len(books)} books in {books_root}, INDEX.md and AGENTS.md written to {root}.")
    if skipped:
        print("Skipped (download these from Dropbox and re-run):")
        for name in skipped:
            print(f"  {name}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
