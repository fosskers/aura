//! The people behind Aura.

use crate::localization::TRANSLATORS;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;

pub(crate) fn thanks(fll: &FluentLanguageLoader) {
    println!("{}", fl!(fll, "thanks-you"));
    println!("{}", fl!(fll, "thanks-pacman"));
    println!("{}", fl!(fll, "thanks-everyone"));
    println!();
    println!("{}", fl!(fll, "thanks-colin"));
    println!("{}", fl!(fll, "thanks-logo"));
    println!("{}", fl!(fll, "thanks-translators"));
    println!();

    let longest = TRANSLATORS
        .iter()
        .map(|(lang, _)| lang.chars().count())
        .max()
        .unwrap_or(0);

    for (lang, guys) in TRANSLATORS {
        println!("{lang:w$} : {guys}", w = longest);
    }
}
