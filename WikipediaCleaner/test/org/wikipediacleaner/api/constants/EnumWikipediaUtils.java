/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.constants;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.contents.magicword.FunctionMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.ImageMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.SimpleMagicWordType;

/**
 * Utility class for initializing wiki configuration
 */
public class EnumWikipediaUtils {

  private EnumWikipediaUtils() {
  }

  /**
   * @return Configured wiki for EN.
   */
  public static EnumWikipedia getEN() {
    EnumWikipedia wiki = EnumWikipedia.EN;

    // Configure name spaces
    List<Namespace> namespaces = new ArrayList<>();
    Namespace categoryNS = new Namespace(
        Integer.toString(Namespace.CATEGORY),
        "Category", "Category",
        EnumCaseSensitiveness.FIRST_LETTER, true);
    namespaces.add(categoryNS);
    Namespace fileNS = new Namespace(
        Integer.toString(Namespace.IMAGE),
        "File", "File",
        EnumCaseSensitiveness.FIRST_LETTER, true);
    fileNS.addAlias("Image");
    namespaces.add(fileNS);
    wiki.getWikiConfiguration().setNamespaces(namespaces);

    // Configure interwikis
    List<Interwiki> interwikis = new ArrayList<>();
    interwikis.add(new Interwiki("en", true, "English", "https://en.wikipedia.org/wiki/$1"));
    interwikis.add(new Interwiki("fr", true, "français", "https://fr.wikipedia.org/wiki/$1"));
    wiki.getWikiConfiguration().setInterwikis(interwikis);

    // Configure languages
    List<Language> languages = new ArrayList<>();
    languages.add(new Language("en", "English"));
    languages.add(new Language("fr", "français"));
    wiki.getWikiConfiguration().setLanguages(languages);

    // Configure magic words
    List<MagicWord> magicWords = new ArrayList<>();
    magicWords.add(new MagicWord(FunctionMagicWordType.PAGE_NAME, Collections.singletonList("PAGENAME"), true));
    magicWords.add(new MagicWord(SimpleMagicWordType.REDIRECT, Collections.singletonList("#REDIRECT"), false));
    List<String> thumbAliases = new ArrayList<>();
    thumbAliases.add("thumb");
    thumbAliases.add("thumbnail");
    magicWords.add(new MagicWord(ImageMagicWordType.IMG_THUMBNAIL, thumbAliases, true));
    magicWords.add(new MagicWord(SimpleMagicWordType.TOC, Collections.singletonList("__TOC__"), false));
    wiki.getWikiConfiguration().setMagicWords(magicWords);

    return wiki;
  }

  /**
   * @return Configured wiki for FR.
   */
  public static EnumWikipedia getFR() {
    EnumWikipedia wiki = EnumWikipedia.FR;

    // Configure name spaces
    List<Namespace> namespaces = new ArrayList<>();
    Namespace categoryNS = new Namespace(
        Integer.toString(Namespace.CATEGORY),
        "Catégorie", "Category",
        EnumCaseSensitiveness.FIRST_LETTER, true);
    namespaces.add(categoryNS);
    Namespace fileNS = new Namespace(
        Integer.toString(Namespace.IMAGE),
        "Fichier", "File",
        EnumCaseSensitiveness.FIRST_LETTER, true);
    fileNS.addAlias("Image");
    namespaces.add(fileNS);
    wiki.getWikiConfiguration().setNamespaces(namespaces);

    // Configure interwikis
    List<Interwiki> interwikis = new ArrayList<>();
    interwikis.add(new Interwiki("en", true, "English", "https://en.wikipedia.org/wiki/$1"));
    interwikis.add(new Interwiki("fr", true, "français", "https://fr.wikipedia.org/wiki/$1"));
    wiki.getWikiConfiguration().setInterwikis(interwikis);

    // Configure languages
    List<Language> languages = new ArrayList<>();
    languages.add(new Language("en", "English"));
    languages.add(new Language("fr", "français"));
    wiki.getWikiConfiguration().setLanguages(languages);

    // Configure magic words
    List<MagicWord> magicWords = new ArrayList<>();
    magicWords.add(new MagicWord(FunctionMagicWordType.PAGE_NAME, Collections.singletonList("PAGENAME"), true));
    magicWords.add(new MagicWord(SimpleMagicWordType.REDIRECT, Collections.singletonList("#REDIRECT"), false));
    List<String> thumbAliases = new ArrayList<>();
    thumbAliases.add("thumb");
    thumbAliases.add("thumbnail");
    magicWords.add(new MagicWord(ImageMagicWordType.IMG_THUMBNAIL, thumbAliases, true));
    magicWords.add(new MagicWord(SimpleMagicWordType.TOC, Collections.singletonList("__TOC__"), false));
    wiki.getWikiConfiguration().setMagicWords(magicWords);

    return wiki;
  }
}
