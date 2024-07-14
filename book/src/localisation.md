# Language Localisation Guide

Welcome! こんにちは！ Saluton!

If you're reading this then it's likely that you want to help localise Aura into
another language. Arch users everywhere can benefit from your contribution, and
it's a great opportunity to contribute to Free Software.

## What You Need

1. [The Aura source code](https://github.com/fosskers/aura).

2. An editor. Whichever one you like.

3. `git`. As Aura is hosted on github, cloning, making changes and adding pull
   requests will be easiest if you have a working git/github setup.

4. A brain (hopefully yours) with a language in it. As for me, no hablo español,
   Я не могу говорить по-русски, nor do I يتكلم العربية, so that's where you
   come in.

## Localisation

The first thing to determine is whether you are extending some existing
translations or writing a new set. If you're extending an existing set, skip to
Step Two.

Aura utilises Mozilla's [Fluent Project](https://projectfluent.org/) for
localisation. The format is text-based and straight-forward, and also allows for
special plural handling. See the [Fluent
Guide](https://projectfluent.org/fluent/guide/) for more information.

In the examples below, we will pretend to add localisations for Latin. 

### Step One - Establish a new Localisation File

You will find all localisation files under `rust/aura-pm/i18n/`. There is a
subdirectory for each language. To add one for Latin, we first [look up its
locale code](https://simplelocalize.io/data/locales/) and find it to be `la-VA`,
associated with Vatican City.

```
mkdir la-VA
cp en-US/aura_pm.ftl la-VA/
```

We have copied over the English translations as a base. 

### Step Two - Translate the English

The top section of the file looks like:

```
language-name = English

# AUR Packages (-A)
A-install-deps = Determining dependencies...
A-install-repo-pkgs = Repository dependencies:
A-install-aur-pkgs = AUR packages:
A-install-path-comp = Failed to extract final component of: { $path }
A-install-ignored = { $file } is marked "ignored". Install anyway?
```

`language-name` is important. This is the output used in `aura stats --lang`. We could make this:

```
language-name = Lingua Latina
```

Then we continue, line by line, replacing the original English with Latin. For
example:

```
A-build-continue = Permanere aliis packages aedificationem?
```

Some lines have "slots" of the format `{ $foo }`, where a value coming from Aura
will be used to complete the message:

```diff
+A-build-e-filename = Defecit e filename eliciunt: { $file }
-A-build-e-filename = Failed to extract filename from: { $file }
```

> **💡 Tip:** If you don't translate every line, Fluent will automatically fall
> back to English for the ones that are missing. No error will occur.

If you add a `#` to the beginning of a line, that will make it a "comment", thus
disabling it.

```
# Note: Translate this after I finish conquering the barbarian hordes.
# A-u-fetch-info = Fetching package information...
```

### Step Three (Optional) - Tell the Rust Code about the New Language

If you're capable with Rust, you can register the new language yourself.
Otherwise, I will do it after you submit your translations.

First, in `rust/aura-pm/src/lib.rs`, we tell the Rust about the new locale:

```rust
pub const LATIN: LanguageIdentifier = langid!("la-VA");
```

Then, within `rust/aura-pm/src/localization.rs`, we update the
`identifier_from_locale` function to include the language code for Latin:

```rust
pub(crate) fn identifier_from_locale<S>(locale: S) -> Option<LanguageIdentifier>
where
    S: AsRef<str>,
{
    match code_and_country(locale.as_ref()) {
        ("en", _) => Some(aura_pm::ENGLISH),
        // ... other languages ...
        ("la", _) => Some(aura_pm::LATIN),
        _ => None,
    }
}
```

Next, we add a command-line flag for Latin within `rust/aura-pm/src/flags.rs`:

```rust
pub struct Args {
    /// Output in Latin (alias: Latinus).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "latinus",
        hide_short_help = true,
        display_order = 10
    )]
    pub latin: bool,
    // ... more flags ...
```

Thanks to the `alias`, we can turn on Latin with either `--latin` or
`--latinus`. Make sure to add both variants to the `AURA_GLOBALS` list. This is
important for interaction with Pacman.

Finally, back in `localization.rs`, we can give credit to ourselves for having
added Latin:

```rust
pub(crate) const TRANSLATORS: &[(&str, &str)] = &[
    // ... other languages ...
    ("Korean", "\"Nioden\""),
    ("Latin", "Maximus Decimus Meridius")
    ("Norwegian", "\"chinatsun\""),
    // ... other languages ...
];
```

Strength and honour, General.

### Step Four - Pull Request

With the translations complete, you'll need to tell us about it on Github.
Once your changes are looked over, we'll release a new version of Aura with
your language included as soon as possible. Provided you followed the above
instructions, this shouldn't take long. Furthermore, chances are we won't be
able to proofread the translation itself, as we probably don't speak your
language. You could hide your doomsday take-over plans in the code and no
one would know.

### Step Six - You've Helped Others who Speak your Language

You've done a great thing by increasing Aura's usability. Your name will be
included in both Aura's README and in its `thanks` message. Thank you for your
hard work!

## Questions

### How do I test my translations?

You will need to do Step Three first. To view your translation progress, you can
run:

```
cargo run -- stats --lang
```

To see your translations in action, run some command:

```
cargo run -- -Au --latin
```

### My language is associated with multiple countries. Which locale code should I choose?

The general rule is "the country it came from". This applies to languages like
Spanish and Portuguese, whose countries established colonies across the world
that now outpopulate their land of origin. German and French as well, which have
strong footholds in Europe across many countries. The exception is English, as
`en-US` is the dominant locale of technology and the internet.

If the country and language name don't match, then pick the place with the most
native speakers. This would apply to languages like Farsi (Persian), primarily
spoken in Iran.

If the language has a language code but not any associated country, like
Esperanto, then invent one (e.g. `eo-EO`). Fluent [won't know the difference](https://github.com/kellpossible/cargo-i18n/issues/52).

If for political reasons you have difficulty recognizing the country currently
associated with the language, we ask you to put those concerns aside at least
for the naming of the directory. Within Aura's code, output messages are
selected based on the language portion of the locale only, not the country, so
you're free to set it as you wish. Further, if you wish to provide separate
localisations for a specific country whose language is shared across many (for
instance, the various `zh` Chinese languages), Aura's code can be altered to
specifically dispatch on that country. Please bring it up when discussing your
submission.

If you're still unsure, please ask.
