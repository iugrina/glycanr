# version 0.4.0

## Initial submission

## Resubmission

This is a resubmission. In this version I have:

* improved URLs in the introduction vignette as requested by CRAN reviewers.

### Test environments
* MacOS
  * R 4.0.4 (2021-02-15) / x86_64-apple-darwin17.0 (64-bit)
* rhub.io
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
* win-builder (win-builder.r-project.org)
  * R-devel (unstable) (2021-03-25 r80117)
  * R 3.6.3 (2020-02-29)
  * R 4.0.4 (2021-02-15)
* travis-ci
  * R-devel (unstable) (2021-03-25 r80120) / x86_64-pc-linux-gnu (64-bit)
  * R version 4.0.2 (2020-06-22) / x86_64-pc-linux-gnu (64-bit)
  * R version 3.6.3 (2020-02-29) / x86_64-pc-linux-gnu (64-bit)

### R CMD check (--as-cran) results
There were no ERRORs and WARNINGs.

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Ivo Ugrina <ivougrina@gmail.com>’

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2019-01-24 as check issues were not
    corrected in time.

# version 0.3.1

## Resubmission #2
This is a resubmission.

* Package names, software names and API names in single quotes (e.g. 'glycanr') in Description.
* Acronyms (e.g. LCMS and UPLC) explaind in Description text to avoid misunderstandings.

## Resubmission
This is a resubmission. In this version I have:

* enclosed the URLs in DESCRIPTION in angle brackets (<...>)

## Initial submission

### Test environments
* Debian, R 3.3.3 (x86_64)
* Debian, R-devel (x86_64)
* win-builder (win-builder.r-project.org)
* travis-ci 
  * R-devel (2018-06-20 r74923)
  * R-release (3.5.0)
  * R-old (3.4.4)

### R CMD check (--as-cran) results
There were no ERRORs and WARNINGs. 

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Ivo Ugrina <ivougrina@gmail.com>’

New submission

Package was archived on CRAN

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2014-2018
  COPYRIGHT HOLDER: Ivo Ugrina

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2018-02-20 as check problems were not
    corrected despite reminders.

  Unconditional use of Suggests package(s) from BioC.

**comment**: The package was archived due to the unconditional
use of Suggests package(s) from BioC in tests. This should
now be resolved.

# version 0.3.0

## Initial submission

### Test environments
* Debian, R 3.2.4 (x86_64)
* Debian, R-devel (x86_64)
* win-builder (win-builder.r-project.org)
* travis-ci 
  * R-devel
  * R-release
  * R-old

### R CMD check (--as-cran) results
There were no ERRORs and WARNINGs. 

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ivo Ugrina <ivo@iugrina.com>'

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2014-2016
  COPYRIGHT HOLDER: Ivo Ugrina

Possibly mis-spelled words in DESCRIPTION:
  Glycan (2:30)
  Glycomics (13:16)
  IgG (14:62)
  LCMS (14:34)
  UPLC (14:25)
  glycan (9:36, 10:26)
  glycanr (13:42)
  glycome (14:66)

**comment**: Words are not mis-spelled and I don't see a problem with the
license.

# version 0.2.0

## Resubmission #2
This is a resubmission. In this version I have:

* improved the description of the package in the DESCRIPTION file

## Resubmission
This is a resubmission. In this version I have:

* Fixed a problem with misuse of LICENSE file

* Resolved problems with the new "include only base package"
  policy within R-devel for check --as-cran

## Initial submission

### R CMD check results
There were no ERRORs and WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Ivo Ugrina <ivo@iugrina.com>’
  New submission


## Test environments
* OS X, R 3.2.2
* Debian, R 3.2.2
* Debian, R-devel
* win-builder (win-builder.r-project.org)

