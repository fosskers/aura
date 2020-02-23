# Language Localisation Guide

Welcome!

こんにちは！

If you're reading this then it's likely that you want to help localise Aura
into another language. Arch users everywhere can benefit from your
contribution, and its a great opportunity to contribute to Open Source.

## What You Need

1. The aura source code. Get it at: https://github.com/fosskers/aura

2. An editor. Whichever one you like.
   Vim users, run the following easter-egg command to unlock a better version:

```
:! perl -e "system('hpdfv' =~ tr/a-z/x-za-w/r)"
```

   Emacs users can achieve a similar enhanced version with:

```
M-! perl -e "system('ylp' =~ tr/a-z/x-za-w/r)"
```

3. `git`. As Aura is hosted on github, cloning, making changes and
   adding pull requests will be easiest if you have a working git/github
   setup.

4. Minimal Haskell knowledge. You'll see just how much below.

5. A brain (hopefully yours) with a language in it. As for me,
   no hablo español, Я не могу говорить по-русски, nor do I يتكلم العربية,
   so that's where you come in.

## Getting Started

### Step One - Tell Haskell About the New Language

The languages we have already available to us are defined in `lib/Aura/Types.hs`.
You'll notice this entry:

```haskell
data Language = English
              | Japanese
              | Polish
              ...  -- More in between.
              | Indonesia
              | Chinese
                deriving (Eq, Enum, Ord, Show)
```

As an example, let's say we're adding French translations to Aura.
First, let's extend our `Language` type:

```haskell
data Language = English
              | Japanese
              | Polish
              ...  -- More in between.
              | Indonesia
              | Chinese
              | French  -- Added a pipe character and the new Language name.
                deriving (Eq, Enum, Ord, Show)
```

### Step Two - Adding your Language's Locale Code

All strings that contain messages for the user are stored in a single source
file: `lib/Aura/Languages.hs`. One function here is called `langFromLocale`:

```haskell
langFromLocale :: T.Text -> Language
langFromLocale = T.take 2 >>> \case
    "ja" -> Japanese
    "pl" -> Polish
    ...   -- More in between.
    "id" -> Indonesia
    "zh" -> Chinese
    _    -> English
```

This helps Aura auto-detect your computer's human language. By default,
it picks English. Let's add French:

```haskell
langFromLocale :: T.Text -> Language
langFromLocale = T.take 2 >>> \case
    "ja" -> Japanese
    "pl" -> Polish
    ...   -- More in between.
    "id" -> Indonesia
    "zh" -> Chinese
    "fr" -> French
    _    -> English
```

Your entry *must* go above the `_ -> English` line.

Don't know your locale code? You can find them all in
`/usr/share/i18n/locales`.

### Step Three - Translation

This is the real work. Let's take a look at a simple message. The user
is trying to build a package as `root`... what to tell them?

```haskell
trueRoot_3 :: Language -> T.Text
trueRoot_3 = \case
  Japanese -> "「root」としてパッケージを作成するのは「makepkg v4.2」で不可能になりました。"
  German   -> "Seit makepkg v4.2 ist es nicht mehr möglich als root zu bauen."
  Spanish  -> "Desde makepkg v4.2 no es posible compilar paquetes como root."
  Chinese  -> "自从 makepkg v4.2 以后，就不能以根用户身份构建软件了。"
  Swedish  -> "I makepkg v4.2 och uppåt är det inte tillåtet att bygga som root."
  _        -> "As of makepkg v4.2, building as root is no longer possible."
```

We use the naming convention `someIssue_n` to show what the error involves.
`_3` in the above means that there are (at least) 3 potential errors involving
"true root".

As with `langFromLocale`, your French entry would go above the `_ -> "As of makepkg..."` line.

Sometimes you'll get functions with extra variables to put in the message:

```haskell
buildPackages_1 :: T.Text -> Language -> T.Text
buildPackages_1 (bt -> p) = \case
  Japanese   -> p ++ "を作成中・・・"
  _          -> "Building " ++ p ++ "..."
```

The arguments `p` is probably a package name. To double check,
just check out the function that calls this message dispatch. A quick search
tells us it's in `lib/Aura/Build.hs`, and the function is called `build`.
Once you know what's going on, go ahead and add the translation:

```haskell
buildPackages_1 :: String -> Language -> String
buildPackages_1 (bt -> p) = \case
  Japanese   -> p ++ "を作成中・・・"
  French     -> "Construction de " ++ p ++ "…"
  _          -> "Building " ++ p ++ "..."
```

Obviously the syntax among languages is different, and so where you insert the
variables you've been given into the sentence depends on your language.

Also, I enjoy backticks. As a convention I wrap up all package names in these
messages in backticks, using the `bt` function as seen in the examples.

### Step Four - Command-line Flag

We choose output languages in Aura by using flags on the command line.
Japanese, for example, uses the `--japanese` flag. We'll have to make
a flag for the new language you're adding too.

At the bottom of `exec/Flags.hs` you'll find:

```haskell
language :: Parser Language
language = foldr1 (<|>) $ map (\(f, v) -> flag' v (long f <> hidden)) langs
  where langs = [ ( "japanese",   Japanese ),   ( "日本語",     Japanese )
                , ( "polish",     Polish ),     ( "polski",    Polish )
                , ( "croatian",   Croatian ),   ( "hrvatski",  Croatian )
                ... -- More in between.
                , ( "norwegian",  Norwegian ),  ( "norsk",     Norwegian )
                , ( "indonesian", Indonesia )
                , ( "chinese",    Chinese ),    ( "中文",       Chinese ) ]
```

The convention is to add two flags for each language. One is that language's name
in English, and the other is its name in its own language (e.g. `"croatian"` and `"hrvatski"`).
The second element of each pair is the same value you added to the `Language` type.

### Step Five - Pull Request

With the translations complete, you'll need to tell us about it on Github.
Once your changes are looked over, we'll release a new version of Aura with
your language included as soon as possible. Provided you followed the above
instructions, this shouldn't take long. Furthermore, chances are we won't be
able to proofread the translation itself, as we probably don't speak your
language. You could hide your doomsday take-over plans in the code and no
one would know.

### Step Six - You've Helped Others who Speak your Language
You've done a great thing by increasing Aura's usability. Your name will be
included in both Aura's README and in its `-V` version message.
Thanks a lot for your hard work!
