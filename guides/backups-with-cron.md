Automatic Package Record Backups with Cron
==========================================

 When upgrading packages, sometimes things just go wrong. I tend to keep on top
of my upgrading and my `.pacnew` files, but even so I'll get a system-crippling
update maybe twice a year - just frequent enough to keep me on my toes.


 `aura -B` comes to the rescue at times like these. Options prefixed with the `-B`
operator manage the saving and restoring of package records. `-B` offers a
light-weight way of backing up your system without doing hard copies of all
the files in your package cache.


 By setting up cron jobs, we can automate the process of saving these records
and clearing out old ones. To set up cron jobs, we need to edit our `crontab`.
A `crontab` is a listing of timings and programs to run. It's a schedule for
our cron jobs. You can edit your user's crontab with `crontab -e`. This will
open your default editor.


 See `man 5 crontab` for more details into the editing of crontabs. For now,
simply copying the following two entries can get you started:

```
# Save a package record at 8pm on Wednesday and Sunday every week.
* 20 * * wed,sun root /usr/bin/aura -B

# Reduce the package record number to 5 once a month at 8:01pm.
1 20 1 * * root /usr/bin/aura -Bc 5 --noconfirm
```

 Since both of these entries would touch the same files, we set them one minute
apart to prevent any problems. With these in place, if any upgrade problems
occur, we know our most recent save was at most only three days ago. 

 Save and exit, and you're all set. Happy package managing!

---

P.S. I also use the following job to keep the size of my package cache down:

```
# Clean out the package cache once a month in the same way.
* 20 1 * * root /usr/bin/aura -Cc 5 --noconfirm
```
