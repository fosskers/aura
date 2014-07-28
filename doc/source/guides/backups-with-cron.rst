==========================================
Automatic Package Record Backups with Cron
==========================================

When upgrading packages, sometimes things just go wrong. I tend to keep
on top of my upgrading and my ``.pacnew`` files, but even so I'll get a
system-crippling update maybe twice a year - just frequent enough to
keep me on my toes.

:command:`aura -B` comes to the rescue at times like these. Options prefixed
with the :command:`-B` operator manage the saving and restoring of package
records. :command:`-B` offers a light-weight way of backing up your system
without doing hard copies of all the files in your package cache.

By setting up cron jobs, we can automate the process of saving these records
and clearing out old ones. To set up cron jobs, we need to edit our
:command:`crontab`. A :command:`crontab` is a listing of timings and programs
to run. It's a schedule for our cron jobs. Since :command:`aura -B` is
typically ran with :command:`sudo`, we'll need to edit root's crontab. You can
do this with :command:`sudo crontab -e`. This will open root's default editor.

See :command:`man 5 crontab` for more details into the editing of crontabs. For
now, simply copying the following two entries can get you started::

   # Save a package record at 8pm on Wednesday and Sunday every week.
   0 20 * * wed,sun /usr/bin/aura -B

   # Reduce the package record number to 5 once a month at 8:01pm.
   1 20 1 * * /usr/bin/aura -Bc 5 --noconfirm

Since both of these entries would touch the same files, we set them one minute
apart to prevent any problems. With these in place, if any upgrade problems
occur, we know our most recent save was at most only three days ago.

Save and exit, and you're all set. Happy package managing!

----

P.S. I also use the following job to keep the size of my package cache
down::

   # Clean out the package cache once a month in the same way.
   0 20 1 * * /usr/bin/aura -Cc 5 --noconfirm

