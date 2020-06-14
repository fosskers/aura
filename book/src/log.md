# Log Interaction

Pacman keeps an extensive log file, but doesn't really offer any way to interact
with it. Aura has the `-L` command to perform some interesting lookups.

## View the Log File

To open the entire log file in `less`:

```
> aura -L
[2016-05-03 04:13] [PACMAN] Running 'pacman -r /mnt -Sy --cachedir=/mnt/var/cache/pacman/pkg base base-devel'
[2016-05-03 04:13] [PACMAN] synchronizing package lists
[2016-05-03 04:26] [ALPM] transaction started
[2016-05-03 04:26] [ALPM] installed linux-api-headers (4.4.1-1)
[2016-05-03 04:26] [ALPM] installed tzdata (2016d-1)
[2016-05-03 04:26] [ALPM] installed iana-etc (20160314-1)
[2016-05-03 04:26] [ALPM] installed filesystem (2015.09-1)
[2016-05-03 04:26] [ALPM] installed glibc (2.23-1)

... etc ...
```

A blast from the past! Looks like a set up Arch on this laptop in 2016.
And what was the most recent thing `pacman` did (press `G`)?

```
... etc ...

[2020-06-14T09:19:43-0700] [ALPM] running 'update-ca-trust.hook'...
[2020-06-14T09:19:51-0700] [ALPM] running 'update-desktop-database.hook'...
[2020-06-14T09:19:51-0700] [ALPM] running 'update-mime-database.hook'...
[2020-06-14T09:19:57-0700] [ALPM] running 'xorg-mkfontscale.hook'...
```

## Search the Log File

`-Ls` returns all log lines that match a given string:

```
> aura -Ls firefox
[2016-05-03 08:45] [PACMAN] Running 'pacman -S firefox'
[2016-05-03 08:46] [ALPM] installed firefox (46.0-2)
[2016-05-07 11:09] [ALPM] upgraded firefox (46.0-2 -> 46.0.1-1)
[2016-06-19 14:30] [ALPM] upgraded firefox (46.0.1-1 -> 47.0-1)
[2016-06-23 15:46] [ALPM] upgraded firefox (47.0-1 -> 47.0-2)
[2016-07-25 07:25] [ALPM] upgraded firefox (47.0-2 -> 47.0.1-1)

... etc ...
```

## Query all Logs for a Package

More interesting than just a raw search is `-Li`:

```
> aura -Li firefox
Package        : firefox
First Install  : 2016-05-03 08:46
Upgrades       : 76
Recent Actions :
[2020-04-08T14:26:09-0700] [ALPM] upgraded firefox (74.0-2 -> 75.0-1)
[2020-05-04T09:20:53-0700] [ALPM] upgraded firefox (75.0-1 -> 75.0-2)
[2020-05-18T08:39:43-0700] [ALPM] upgraded firefox (75.0-2 -> 76.0.1-1)
[2020-06-12T11:39:58-0700] [ALPM] downgraded firefox (76.0.1-1 -> 75.0-2)
[2020-06-12T11:41:03-0700] [ALPM] upgraded firefox (75.0-2 -> 77.0.1-1)
```

Neat! I often use this to check the last time I updated a particular package.
