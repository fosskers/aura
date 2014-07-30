===========================
Language Localisation Guide
===========================

Welcome!

こんにちは！

If you're reading this then it's likely that you want to help localise Aura
into another language. Arch users everywhere can benefit from your
contribution, and its a great opportunity to contribute to Open Source.

What You Need
=============

1. The aura source code. Get it at: https://github.com/aurapm/aura

2. An editor. Whichever one you like. 
 
   Vim users, run the following easter-egg command to unlock a better version::

      :! perl -e "system('hpdfv' =~ tr/a-z/x-za-w/r)"

   Emacs users can achieve a similar enhanced version with::

      M-! perl -e "system('ylp' =~ tr/a-z/x-za-w/r)"

3. :command:`git`. As Aura is hosted on github, cloning, making changes and
   adding pull requests will be easiest if you have a working git/github
   setup.

4. Minimal Haskell knowledge. You'll see just how much below.

5. A brain (hopefully yours) with a language in it. As for me,
   no hablo español, Я не могу говорить по-русски, nor do I يتكلم العربية,
   so that's where you come in.

Getting Started
===============

Step One - Tell Haskell About the New Language
----------------------------------------------

All strings that contain messages for the user are stored in a single source
file: :file:`src/Aura/Languages.hs`. Let's take a look at the top::

    data Language = English
                  | Japanese
                    deriving (Eq,Enum,Show)

This is where we define output languages for Aura. For the purpose of
demonstration, we'll use ``French`` as the language we're adding. Add a
new language by adding a new value to the Language data type. Like this::

    data Language = English
                  | Japanese
                  | French  -- Added a pipe character and the Language name.
                    deriving (Eq,Enum,Show)

Step Two - Adding your Language's Locale Code
---------------------------------------------

See the function ``langFromEnv``. It is given the contents of the
environment variable ``LANG``, and checks the first two characters,
returning the appropriate name of the language entered in the
``Language`` field above. English is the default.

::

    langFromEnv :: String -> Language
    langFromEnv = \case
        "ja" -> Japanese
        _    -> English

For French, we would add a new field above ``English``.

::

    langFromEnv :: String -> Language
    langFromEnv = \case
        "ja" -> Japanese
        "fr" -> French
        _    -> English

Don't know your locale code? You can find them all in
:file:`/usr/share/i18n/locales`.

Step Three - Translation
------------------------

This is the real work. Let's take a look at a simple message. The user
has passed some bogus/conflicting flags to Aura. What to tell them?

::

    -----------------
    -- aura functions
    -----------------
    executeOpts_1 :: Language -> String
    executeOpts_1 = \case
        Japanese -> "矛盾しているオプションあり。"
        _        -> "Conflicting flags given!"

All functions in Aura code that output messages to the user get that
message with a dispatch. That is, they call a function with the current
language they're using, and that function returns the appropriate
message.

Notice the handy label in the comment there. This tells *where* in the Aura
code the calling function is located. If you ever need more context as to what
kind of message you're writing, checking the code directly will be quickest.
The format is::

   SomeLanguage -> "The message."

This naming is nothing more than a convention. So let's go ahead and add the
French message::

    ----------------
    -- aura functions
    ----------------
    executeOpts_1 :: Language -> String
    executeOpts_1 = \case
        Japanese -> "矛盾しているオプションあり。"
        French   -> "Arguments contradictoires!"
        _        -> "Conflicting flags given!"

Sometimes you'll get functions with extra variables to put in the message::

    -----------------------
    -- Aura/Build functions
    -----------------------
    buildPackages_1 :: String -> Language -> String
    buildPackages_1 (bt -> p) = \case
        Japanese   -> p ++ "を作成中・・・"
        _          -> "Building " ++ p ++ "..."

What the heck is ``p``? Well it's probably a package name. To double check,
just check out the function that calls this message dispatch. We know it's in
:file:`src/Aura/Build.hs`, and the function is called ``buildPackages``. Once
you know what's going on, go ahead and add the translation::

    -----------------------
    -- Aura/Build functions
    -----------------------
    buildPackages_1 :: String -> Language -> String
    buildPackages_1 (bt -> p) = \case
        Japanese   -> p ++ "を作成中・・・"
        French     -> "Construction de " ++ p ++ "…"
        _          -> "Building " ++ p ++ "..."

Obviously the syntax among languages is different, and so where you insert the
variables you've been given into the sentence depends on your language.

Also, I enjoy backticks. As a convention I wrap up all package names in these
messages in backticks, using the ``bt`` function as seen in the examples. This
also colours them cyan.

Step Four - Command-line Flag
-----------------------------

We choose output languages in Aura by using flags on the command line.
Japanese, for example, uses the :command:`--japanese` flag. We'll have to make
a flag for the new language you're adding too.

This step is not actually necessary for you to do... so long as the
translations are done I can take care of the rest of the code editing.  But for
the interested, edit :file:`src/Aura/Flags.hs`::

    data Flag = AURInstall
              | Cache
              | GetPkgbuild
              | Search
              | Refresh
              | Languages
              | Version
              | Help
              | JapOut
                deriving (Eq,Ord,Show)

You could add French like this::

    data Flag = AURInstall
              | Cache
              | GetPkgbuild
              | Search
              | Refresh
              | Languages
              | Version
              | Help
              | JapOut
              | FrenchOut  -- Here.
                deriving (Eq,Ord,Show)

Then we need to add it to the options to be checked for, edit
:file:`Aura/Flags.hs`::

    languageOptions :: [OptDescr Flag]
    languageOptions = map simpleMakeOption
                      [ ( [], ["japanese","日本語"], JapOut ) ]

...would thus become::

    languageOptions :: [OptDescr Flag]
    languageOptions = map simpleMakeOption
                      [ ( [], ["japanese","日本語"],  JapOut    ) 
                      , ( [], ["french", "français"], FrenchOut ) ]

Notice how each language has two long options. Please feel free to add
your language's *real* name in its native characters.

Last step in the flag making::

    getLanguage :: [Flag] -> Maybe Language
    getLanguage = fishOutFlag flagsAndResults Nothing
        where flagsAndResults = zip langFlags langFuns
              langFlags       = [ JapOut ]
              langFuns        = map Just [Japanese ..]

This function extracts your language selection from the rest of the
options. Let's add French.

::

    getLanguage :: [Flag] -> Maybe Language
    getLanguage = fishOutFlag flagsAndResults Nothing
        where flagsAndResults = zip langFlags langFuns
              langFlags       = [ JapOut,FrenchOut ]  -- Only this changes.
              langFuns        = map Just [Japanese ..]

Where ``FrenchOut`` is the value you added to ``Flags`` above.

Step Five - Pull Request
------------------------

With the translations complete, you'll need to tell us about it on github.
Once your changes are looked over, we'll release a new version of Aura with
your language included as soon as possible. Provided you followed the above
instructions, this shouldn't take long. Furthermore, chances are we won't be
able to proofread the translation itself, as we probably don't speak your
language. You could hide your doomsday take-over plans in the code and no
one would know.

Step Six - You've Helped Others who Speak your Language
-------------------------------------------------------
You've done a great thing by increasing Aura's usability. Your name will be
included in both Aura's README and in its :command:`-V` version message.
Thanks a lot for your hard work!
