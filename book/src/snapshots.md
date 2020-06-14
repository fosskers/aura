# Saving Package Set Snapshots

Arch Linux (and Linux in general) used to have a reputation of breaking every
time packages were upgraded. Around 2010, Aura's author used to run Arch on a
Macbook, and such catastrophic breakage *did* occur every six months or so. But
fast-forward to the present day, and such problems are rare. Even so, the idea
of "whole system rollback" is spreading, and even since those early Arch-on-Mac
days, Aura has had the `-B` Command for saving and restoring entire package
sets.

## Saving a Package Set

```
> sudo aura -B
aura >>= Saved package state.
```

This saves a file like `2020.06(Jun).14.08.56.46.json` to
`/var/cache/aura/states/`. If we take a peek inside, we see:

```
> cat 2020.06\(Jun\).14.08.56.46.json | aeson-pretty | head
{
    "time": "2020-06-14T08:56:46.437911573-07:00",
    "pinned": false,
    "packages": {
        "rasqal": "1:0.9.33-3",
        "java-environment-common": "3-3",
        "kwallet": "5.70.0-1",
        "python2-livereload": "2.6.1-1",
        "nethogs": "0.8.6-1",
        "coreutils": "8.32-1",
```

Simple enough - a list of all installed packages with their versions. We'll talk
about `pinned` below.

These files are in JSON format in case other tools wish to read them.

## Restoring a Package Set

I performed a big `-Syu` of about 250 packages, but wish to roll them all back
to the saved state I just made above. Let's see that happen:

```
> sudo aura -Br
 1. /var/cache/aura/states/2018.07(Jul).06.10.03.18.json
 2. /var/cache/aura/states/2020.05(May).04.09.15.57.json
 3. /var/cache/aura/states/2020.05(May).11.11.30.35.json

... many choices ...

49. /var/cache/aura/states/2020.06(Jun).12.10.23.12.json
50. /var/cache/aura/states/2020.06(Jun).14.08.56.46.json
51. /var/cache/aura/states/2020.06(Jun).14.09.03.59.json
>> 51
aura >>= Requested downgrade versions not available for:
lib32-libdrm
libdrm
libretro-nestopia
linux-firmware
portaudio
runc
spandsp
virtualbox
vlc
xine-lib

resolving dependencies...
looking for conflicting packages...

Package (234)                 Old Version             New Version           Net Change

acorn                         1:7.3.1-1               1:7.2.0-1               0.00 MiB
alsa-lib                      1.2.3-1                 1.2.2-1                -0.01 MiB
alsa-topology-conf            1.2.3-1                 1.2.2-2                -0.19 MiB
alsa-ucm-conf                 1.2.3-1                 1.2.2-1                -0.02 MiB
alsa-utils                    1.2.3-1                 1.2.2-1                 0.00 MiB

... etc ...

wine                          5.10-1                  5.9-1                  -1.26 MiB
x265                          3.4-1                   3.3-1                  -0.01 MiB
xkeyboard-config              2.30-1                  2.29-1                  0.09 MiB

Total Installed Size:  3315.10 MiB
Net Upgrade Size:       -38.46 MiB

:: Proceed with installation? [Y/n]

... downgrading ...
```

It all went smoothly, even rolling back the kernel. Notice that Aura will warn
you if it couldn't find certain versions to roll back to.

## Clearing out old Saved States

50 saved states is a little much. I probably don't even have most of the
packages necessary to roll back to those early states, since I use `-Cc` often.
Let's clear out the old ones:

```
> sudo aura -Bc 5
aura >>= You currently have 52 saved package states.
aura >>= 1 of these are pinned, and won't be removed.
aura >>= Most recently saved: 2020.06(Jun).14.09.17.09.json
aura >>= 5 package states will be kept. Remove the rest? [Y/n]
```

And sure enough, only the most recent remain:

```
> sudo aura -Br
1. /var/cache/aura/states/2018.07(Jul).06.10.03.18.json
2. /var/cache/aura/states/2020.06(Jun).12.10.22.56.json
3. /var/cache/aura/states/2020.06(Jun).12.10.23.12.json
4. /var/cache/aura/states/2020.06(Jun).14.08.56.46.json
5. /var/cache/aura/states/2020.06(Jun).14.09.03.59.json
6. /var/cache/aura/states/2020.06(Jun).14.09.17.09.json
>>
```

Hey wait, what is that state from 2018 still doing there? Notice:

> aura >>= 1 of these are pinned, and won't be removed.

Now we can talk about pinning.

## Pinning a Saved State

If we don't want a certain package state to be removed by `-Bc` no matter what,
we can pin it. There is no flag for this, we need to open the `.json` file of
the state we wish to pin, and change its `pinned` field from `false` to `true`.
