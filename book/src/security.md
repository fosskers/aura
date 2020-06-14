# PKGBUILD Security Analysis

In 2018 July, the [Acroread package was
compromised](https://lists.archlinux.org/pipermail/aur-general/2018-July/034151.html).
While the offending commit is no longer available, the mailing list mentions the
cause: someone had injected a line into Acroread's PKGBUILD that `curl`s down
some custom code from the internet and pipes it into a new `bash` process. Many
people discovered that they had been victim to this vulnerability, and it came
without warning and left no obvious trace.

The top of [the AUR page](https://aur.archlinux.org/) states:

> DISCLAIMER: AUR packages are user produced content. Any use of the provided
> files is at your own risk.

"Caveat emptor". And it is considered standard practice to always check
PKGBUILDs before building a package. That said, people aren't perfect, and can
miss details. Should they be punished for that with a system vulnerability?
Aura's author doesn't think so.

Since 2018 August (Aura 2.0), Aura has had automatic bash vulnerability
detection. After dependency checking, each PKGBUILD is parsed and scanned for
malicious bash patterns. If one is detected, you'll see a message like this:

```
> sudo aura -A myget
aura >>= Determining dependencies...

aura >>= WARNING: The PKGBUILD of myget contains blacklisted bash expressions.

    sudo pacman -S aurvote

aura >>= sudo indicates that someone may be trying to gain root access to your machine.
aura >>= Do you wish to quit the build process? [Y/n]
aura >>= Cancelled further processing to avoid potentially malicious bash code.
```

Clearly a PKGBUILD should **not** be calling `sudo pacman` on its own. This is a
clear violation of best practices. To be fair, the author of this PKGBUILD is
probably not malicious, but the package must be fixed either way.

Aside from automatic detection, as of 2020 May (Aura 3.0) Aura also has the `-P`
command which can be used to scan any PKGBUILD you give it.

> **💡 Note:** The presence of automatic PKGBUILD scanning is not an excuse to
> be lazy! Please continue to check PKGBUILDs yourself!

## Scanning a PKGBUILD from Stdin

Wondering about the safety of a particular package on the AUR? We don't have to
try and build it - we can scan the PKGBUILD in isolation:

```
> aura -Ap myget | aura -P

    sudo pacman -S aurvote

aura >>= sudo indicates that someone may be trying to gain root access to your machine.
aura >>= Potential PKGBUILD vulnerabilities detected.
```

Recall that `-Ap` pulls a PKGBUILD from the AUR and prints it to the terminal.

## Scanning a PKGBUILD File

`-P` is also intended as a tool for AUR package maintainers, to help them insure
that they aren't unintentionally doing something dangerous or suspicious. Let's check
Aura's own PKGBUILD...

```
> aura -Pf PKGBUILD
>
```

No error code! Phew...

## Scanning a Directory

You can also indicate a directory that you know contains a file named
`PKGBUILD`:

```
> aura -Pd aura/
>
```

Safe again.
