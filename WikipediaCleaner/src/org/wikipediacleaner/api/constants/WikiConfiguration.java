/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.HttpUtils;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;


/**
 * Configuration for the wiki.
 */
public class WikiConfiguration {

  // ==========================================================================
  // General information
  // ==========================================================================

  /**
   * Server URL (without protocol).
   */
  private String server;

  /**
   * Article path.
   */
  private String articlePath;

  /**
   * Script.
   */
  private String script;

  /**
   * Test if an URL matches an article.
   * 
   * @param url URL to be tested.
   * @return Article if the URL matches an article.
   */
  public String isArticleUrl(String url) {
    if ((url == null) || (server == null)) {
      return null;
    }

    String article = HttpUtils.getArticleFromUrl(url, server + articlePath);
    if (article != null) {
      return article;
    }
    if (script != null) {
      article = HttpUtils.getArticleFromUrl(url, server + script + "?title=$1");
      if (article != null) {
        return article;
      }
    }

    return null;
  }

  /**
   * @param server Server URL (without protocol).
   */
  public void setServer(String server) {
    this.server = server;
  }

  /**
   * @param path Article path.
   */
  public void setArticlePath(String path) {
    this.articlePath = path;
  }

  /**
   * @param script Script.
   */
  public void setScript(String script) {
    this.script = script;
  }

  // ==========================================================================
  // Name spaces
  // ==========================================================================

  /**
   * Namespaces.
   */
  private List<Namespace> namespaces;

  /**
   * @return List of namespaces
   */
  public List<Namespace> getNamespaces() {
    return namespaces;
  }

  /**
   * @param namespaces List of namespaces
   */
  public void setNamespaces(List<Namespace> namespaces) {
    this.namespaces = namespaces;
    if (namespaces != null) {
      Collections.sort(namespaces);
    }
  }

  /**
   * @param id Namespace id.
   * @return Matching namespace.
   */
  public Namespace getNamespace(int id) {
    if (namespaces == null) {
      return null;
    }
    for (Namespace n : namespaces) {
      if ((n != null) && (n.getId() != null) && (id == n.getId().intValue())) {
        return n;
      }
    }
    return null;
  }

  /**
   * @param namespaceId Namespace id.
   * @param title Page title in the namespace.
   * @return Full page title.
   */
  public String getPageTitle(int namespaceId, String title) {
    if (namespaceId == Namespace.MAIN) {
      return title;
    }
    Namespace namespace = getNamespace(namespaceId);
    if (namespace != null) {
      int colonIndex = title.indexOf(':');
      if (colonIndex > 0) {
        String possibleNamespace = title.substring(0, colonIndex);
        if (namespace.isPossibleName(possibleNamespace)) {
          title = title.substring(colonIndex + 1);
        }
      }
      return namespace.getTitle() + ":" + namespace.getCaseSensitiveness().normalize(title);
    }
    return title;
  }

  // ==========================================================================
  // Languages
  // ==========================================================================

  /**
   * Languages
   */
  private List<Language>  languages;

  /**
   * @return List of languages
   */
  public List<Language> getLanguages() {
    return languages;
  }

  /**
   * @param languages List of languages
   */
  public void setLanguages(List<Language> languages) {
    this.languages = languages;
  }

  // ==========================================================================
  // Interwikis
  // ==========================================================================

  /**
   * Interwikis
   */
  private List<Interwiki> interwikis;

  /**
   * @return List of interwikis
   */
  public List<Interwiki> getInterwikis() {
    return interwikis;
  }

  /**
   * @param interwikis List of interwikis
   */
  public void setInterwikis(List<Interwiki> interwikis) {
    this.interwikis = interwikis;
  }

  // ==========================================================================
  // Magic words
  // ==========================================================================

  /**
   * Magic words.
   */
  private Map<String, MagicWord> magicWords;

  /**
   * @param name Magic word name.
   * @return Magic word.
   */
  public MagicWord getMagicWordByName(String name) {
    if ((name == null) || (magicWords == null)) {
      return null;
    }
    return magicWords.get(name); 
  }

  /**
   * @param value Magic word value.
   * @return Magic word.
   */
  public MagicWord getMagicWordByValue(String value) {
    if ((value == null) || (magicWords == null)) {
      return null;
    }
    for (MagicWord magicWord : magicWords.values()) {
      if (magicWord.isPossibleAlias(value)) {
        return magicWord;
      }
    }
    return null; 
  }

  /**
   * @param text Text
   * @param colon True if a colon can be added to the text.
   * @return Matching Magic Word if the text is an alias for a Function Magic Word.
   */
  public MagicWord getFunctionMagicWord(String text, boolean colon) {
    List<String> functionMagicWords = MagicWord.getFunctionMagicWords();
    String colonText = text + ":";
    for (String functionMagicWord : functionMagicWords) {
      MagicWord magicWord = getMagicWordByName(functionMagicWord);
      if (magicWord != null) {
        if (magicWord.isPossibleAlias(text)) {
          return magicWord;
        }
        if (colon && magicWord.isPossibleAlias(colonText)) {
          return magicWord;
        }
      }
    }
    return null;
  }

  /**
   * @param text Text
   * @return Matching Magic Word if the text is an alias for a Image Magic Word.
   */
  public MagicWord getImgMagicWord(String text) {
    List<String> imgMagicWords = MagicWord.getImgMagicWords();
    for (String imgMagicWord : imgMagicWords) {
      MagicWord magicWord = getMagicWordByName(imgMagicWord);
      if ((magicWord != null) && magicWord.isPossibleAlias(text)) {
        return magicWord;
      }
    }
    return null;
  }

  /**
   * @param magicWords Magic words.
   */
  public void setMagicWords(Map<String, MagicWord> magicWords) {
    this.magicWords = magicWords;
  }
}
