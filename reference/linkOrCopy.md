# Create link to file, falling back to making a copy if linking fails.

First try to create a hardlink to the file. If that fails, try a
symbolic link (symlink) before falling back to copying the file. "File"
here can mean a file or a directory.

## Usage

``` r
linkOrCopy(from, to, allowSymlink = FALSE)

fileRenameOrMove(from, to)
```

## Arguments

- from, to:

  character vectors, containing file names or paths.

- allowSymlink:

  Logical. If `FALSE`, the default, then it will try `file.link` first,
  then `file.copy`, omitting the `file.symlink` step
