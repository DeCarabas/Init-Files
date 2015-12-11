function new-link($link, $target) {
  if (test-path $target -PathType Container) {
    cmd /c mklink /j $link $target
  } else {
    cmd /c mklink /h $link $target
  }
}

$ignore = @(".gitignore", "setup.ps1", "setup.cmd")

Get-ChildItem . |
  ? { !$ignore.Contains($_.Name) } |
  % {
    $l = split-path -leaf $_.FullName
    $p = split-path -parent (split-path -parent $_.FullName)
    $t = join-path $p $l

    if (test-path $t) {
      remove-item -recurse -force $t
    }

    new-link $t $($_.FullName)
  }
