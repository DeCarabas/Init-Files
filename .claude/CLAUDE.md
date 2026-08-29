- For all tasks use your judgement to decide an appropriate lower power model and run that in a subagent. As of 7/31/2026 Opus 5 is very good at building software, almost as good as you, so if you're considering writing some code you should strongly consider using Opus 5. (And you should bug me to re-visit this advice if it seems like it might be out of date.)

# Writing Code

- ALWAYS use a red/green testing discipline when writing code: tests first, then make the tests pass.

- Try not to accept compiler errors or "missing member exception" failures as your red status. They may be unavoidable, but are very low signal. A quality red state genuinely shows the defect being corrected or the lacking feature you're developing.

- You are never done until whatever you're working on compiles cleanly and all relevant tests pass. When working with rust code, we're never done until the tests all pass and clippy is happy.

- Be sure to check your progress regularly with builds and tests. This should be in alignment with the red/green discipline above.

- When fixing bugs, do your best to write a test that demonstrates the bug first, both to make sure you have a correct understanding of the problem AND to give you something to measure success against later.

- When commenting code, the comments must be timeless. Mark subtleties of the chosen algorithm, or pitfalls to avoid, but do not make any references to, say, a bug that was just fixed, or the way the code used to be broken, or anything like that. The repository history can surface those. (The only time to mention the past in a comment is if there was, say, another design alternative that was explored and then discarded, OR, say, when there is some vestigial leftover for compatibility.)

e.g., this is a bad comment, introduced after the build tooling fixed a bug in concurrent nuget package restore:

```
      # A build step before the test step, purely so build breaks and test
      # failures land in separate steps with separate timings. Runs at full
      # parallelism: nuget packages are consumed in place from the vendored
      # third-party/nuget tree (a fallbackPackageFolder — see
      # toolchains/dotnet/rules.bzl), so there is no shared package cache to
      # race on, cold container or not.
```

Better:

```
      # A build step before the test step, purely so build breaks and test
      # failures land in separate steps with separate timings.
```

(Why even talk about concurrency here? It is irrelevant for the code at hand.)

- Always make changes in a worktree.

- Never commit work that you want merged to the worktree without explicit permission, so that the work can be reviewed locally. Once reviewed and/or given explicit permisison you may commit and continue.

- Worktrees are merged to main via fast-forward/rebase, never via a merge commit. Always rebase before merging. NEVER MERGE TO MAIN WITHOUT EXPLICIT CONFIRMATION.

# Reviewing Code

- When reviewing code, if you see a bug, just tell me the bug, as tersely as you can. Don't describe the bug in detail or suggest a fix- I will ask for more information if necessary. A good review comment is:

```
Bug: typo at Player.cs:155. The linear branch of PeriodToFrequency uses Period (capital P, the VoiceState field) instead of period (the parameter)
```

A BAD review comment is:

```
Bug: typo at Player.cs:155. The linear branch of PeriodToFrequency uses Period (capital P, the VoiceState field) instead of period (the parameter).

   case TuningMode.LinearFrequency:
      return 8363.0f * MathF.Pow(2.0f, (4608 - Period) / 768);

Should be (4608 - period) / 768. As written, the linear branch ignores its argument and uses this.Period (the base period field), which means the Frequency getter at line 141 — which passes EffectivePeriod in — would silently compute the frequency from the un-modulated base period in linear mode. All vibrato/arpeggio/glissando modulation would vanish from the audio output. The AmigaPeriod branch above it correctly uses lowercase period, so the inconsistency is easy to spot once you look.
```

- Answer questions as tersely as you can. Assume that I will understand the terms that you use, and I will ask for clarification if I do not understand. Likewise, keep your justifications complete but otherwise terse- I will ask you to elaborate if they don't make sense.

# C# guidelines

- Member order is, from top to bottom: fields, constructors, properties, events, methods, nested types. Within sections, order members by visibility: public, internal, protected, private.

- Never explicitly specify the default visibility: methods are private by default, you don't need to write "private", etc.

# Final Notes

- A LITTLE poetry is always worthwhile. Also, "a little nonsense, now and then, is relished by the wisest men," as they say. Take things seriously, and be professional and honest, but also, you know... don't over-do it. A little goes a long way.
