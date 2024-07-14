# Log Interaction

Pacman keeps an extensive log file, but doesn't really offer any way to interact
with it. Aura has the `-L` command to perform some interesting lookups.

## View the Log File

To print the content of the entire log file:

```
> aura -L | bat
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

A blast from the past! Looks like I set up Arch on this laptop in 2016.
And what was the most recent thing that happened (pressing `G`)?

```
... etc ...
[2024-07-14T13:53:16+0900] [ALPM] transaction completed
[2024-07-14T13:53:16+0900] [ALPM] running '30-systemd-daemon-reload-system.hook'...
[2024-07-14T13:53:16+0900] [ALPM] running '30-systemd-update.hook'...
[2024-07-14T13:53:16+0900] [ALPM] running 'gtk-update-icon-cache.hook'...
[2024-07-14T13:53:16+0900] [ALPM] running 'update-desktop-database.hook'...
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
Name           : firefox
First Install  : 2016-05-03 08:46
Upgrades       : 176
Recent Actions : 
[2024-02-24T07:29:46+0900] [ALPM] upgraded firefox (122.0.1-1 -> 123.0-1)
[2024-03-11T16:42:37+0900] [ALPM] upgraded firefox (123.0-1 -> 123.0.1-1)
[2024-03-24T15:03:33+0900] [ALPM] upgraded firefox (123.0.1-1 -> 124.0.1-1)
[2024-06-03T05:18:15+0900] [ALPM] upgraded firefox (124.0.1-1 -> 126.0.1-1)
[2024-06-29T13:17:29+0900] [ALPM] upgraded firefox (126.0.1-1 -> 127.0.2-1)
```

Neat! I often use this to check the last time I updated a particular package.
