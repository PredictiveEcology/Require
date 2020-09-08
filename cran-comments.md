## Updated release

This is a feature enhancement and bugfix release. 
See `NEWS.md` for complete list of changes.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '3.6'
- os: ubuntu-18.04,   r: 'devel', 
- os: ubuntu-18.04,   r: 'release'
- os: ubuntu-18.04,   r: 'oldrel'
- os: ubuntu-18.04,   r: '3.5'
          
### Winbuilder -- all passed Sept 8, 2020
* Windows                 (win-builder), R 3.6.3
* Windows                 (win-builder), R 4.0.2
* Windows                 (win-builder), R 4.1.0

### rhub -- all passed Sept 8, 2020
* Fedora Linux                      (clang, gfortran)
* Windows     (Windows Server 2008 R2 SP1, 32/64 bit)
* Ubuntu Linux            (16.04 LTS, R-release, GCC)

## R CMD check results

There were no ERRORs, or WARNINGs.  The only NOTE concerns the "unable to verify current time".

```
* checking for future file timestamps ... NOTE
unable to verify current time
```

## Downstream dependencies

We have tested this verison of the package with several downstream dependencies, and have found no problems.
