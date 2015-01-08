---
title: CommonQt
layout: wiki
---

[CommonQt][commonqt] is a binding to the [Qt GUI library][qt] built using [Smoke][smoke].

[commonqt]: http://common-lisp.net/project/commonqt/
[qt]: http://qt-project.org/
[smoke]: https://techbase.kde.org/Development/Languages/Smoke

# Libraries

A list of libraries using CommonQt or building upon it:

[Qtools](https://github.com/Shinmera/qtools)
: A collection of utilities for building CommonQt applications.

# Building

CommonQt uses Smoke, so that needs to be installed for it to be built.

## Arch Linux

If you're on Arch, you can install Smoke by installing the
`kdebindings-smokegen` and `kdebindings-smokeqt` packages:

~~~
$[ eudoxia@laptop ] ~
$> sudo pacman -S kdebindings-smokegen kdebindings-smokeqt
resolving dependencies...
looking for inter-conflicts...

Packages (4): qimageblitz-0.0.6-4  qscintilla-2.8.4-3  kdebindings-smokegen-4.14.3-1  kdebindings-smokeqt-4.14.3-1

Total Download Size:    4.22 MiB
Total Installed Size:   23.28 MiB

:: Proceed with installation? [Y/n] y
:: Retrieving packages ...
 kdebindings-smokegen-4.14.3-1-x86_64                  348.4 KiB   223K/s 00:02 [#############################################] 100%
 qimageblitz-0.0.6-4-x86_64                             81.7 KiB  2.16M/s 00:00 [#############################################] 100%
 qscintilla-2.8.4-3-x86_64                            1043.2 KiB   151K/s 00:07 [#############################################] 100%
 kdebindings-smokeqt-4.14.3-1-x86_64                     2.8 MiB   219K/s 00:13 [#############################################] 100%
(4/4) checking keys in keyring                                                  [#############################################] 100%
(4/4) checking package integrity                                                [#############################################] 100%
(4/4) loading package files                                                     [#############################################] 100%
(4/4) checking for file conflicts                                               [#############################################] 100%
(4/4) checking available disk space                                             [#############################################] 100%
(1/4) installing kdebindings-smokegen                                           [#############################################] 100%
(2/4) installing qimageblitz                                                    [#############################################] 100%
(3/4) installing qscintilla                                                     [#############################################] 100%
(4/4) installing kdebindings-smokeqt                                            [#############################################] 100%
$[ eudoxia@laptop ] ~
$>
~~~

# Users

* [Parasol](https://github.com/Shinmera/parasol)
