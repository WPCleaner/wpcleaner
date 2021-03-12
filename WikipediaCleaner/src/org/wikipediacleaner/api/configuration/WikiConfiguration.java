/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.configuration;

import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.SpecialPage;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.MagicWordType;


/**
 * Configuration for the wiki.
 */
public class WikiConfiguration {

  // ==========================================================================
  // General information
  // ==========================================================================

  /** Server URL (without protocol) */
  private String server;

  /** Article path */
  private String articlePath;

  /** Script */
  private String script;

  /** Max article size */
  private Long maxArticleSize;

  /** True if article is translatable */
  private boolean translatable;

  /**
   * @param server Server URL (without protocol).
   */
  public void setServer(String server) {
    this.server = server;
  }

  /**
   * @return Server URL (without protocol).
   */
  public String getServer() {
    return server;
  }

  /**
   * @param path Article path.
   */
  public void setArticlePath(String path) {
    this.articlePath = path;
  }

  /**
   * @return Article path.
   */
  public String getArticlePath() {
    return articlePath;
  }

  /**
   * @param script Script.
   */
  public void setScript(String script) {
    this.script = script;
  }

  /**
   * @return Script.
   */
  public String getScript() {
    return script;
  }

  /**
   * @param size Max article size in bytes.
   */
  public void setMaxArticleSize(String size) {
    maxArticleSize = null;
    try {
      maxArticleSize = Long.valueOf(size);
    } catch (NumberFormatException e) {
      // Nothing to do
    }
  }

  /**
   * @return Max article size in bytes.
   */
  public Long getMaxArticleSize() {
    return maxArticleSize;
  }

  /**
   * @param text Text to check.
   * @return True if text doesn't respect the max article size.
   */
  public boolean isArticleTooLong(String text) {
    if ((maxArticleSize == null) || (text == null)) {
      return false;
    }
    try {
      if (text.getBytes("UTF-8").length > maxArticleSize.longValue()) {
        return true;
      }
    } catch (UnsupportedEncodingException e) {
      // Nothing to do
    }
    return false;
  }

  /**
   * @param translatable True if wiki is translatable.
   */
  public void setTranslatable(boolean translatable) {
    this.translatable = translatable;
  }

  /**
   * @return True if wiki is translatable.
   */
  public boolean isTranslatable() {
    return translatable;
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
    if (title == null) {
      return null;
    }
    int colonIndex = title.indexOf(':');
    if (colonIndex == 0) {
      return title.substring(1);
    }
    Namespace namespace = getNamespace(namespaceId);
    if (namespace != null) {
      if (namespaceId == Namespace.MAIN) {
        return namespace.getCaseSensitiveness().normalize(title);
      }
      if (colonIndex > 0) {
        String possibleNamespace = title.substring(0, colonIndex);
        for (Namespace n : namespaces) {
          if (n.isPossibleName(possibleNamespace)) {
            return n.getTitle() + ":" + n.getCaseSensitiveness().normalize(title.substring(colonIndex + 1));
          }
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
  private Map<MagicWordType, MagicWord> magicWords = new HashMap<>();

  /**
   * @param type Magic word type.
   * @return Magic word.
   */
  public MagicWord getMagicWordByType(MagicWordType type) {
    if ((type == null) || (magicWords == null)) {
      return null;
    }
    return magicWords.get(type); 
  }

  /**
   * @param value Magic word alias.
   * @return Magic word.
   */
  public MagicWord getMagicWordByAlias(String value) {
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
    String colonText = text + ":";
    for (MagicWord magicWord : magicWords.values()) {
      if ((magicWord != null) && (magicWord.isFunctionMagicWord())) {
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
    for (MagicWord magicWord : magicWords.values()) {
      if ((magicWord != null) && (magicWord.isImageMagicWord())) {
        if (magicWord.isPossibleAlias(text)) {
          return magicWord;
        }
      }
    }
    return null;
  }

  public void setMagicWords(List<MagicWord> magicWords) {
    if (magicWords == null) {
      this.magicWords = Collections.emptyMap();
    } else {
      this.magicWords = magicWords.stream().collect(Collectors.toMap(MagicWord::getType, mw -> mw));
    }
  }

  // ==========================================================================
  // Special pages
  // ==========================================================================

  /**
   * Special pages.
   */
  private Map<String, SpecialPage> specialPages;

  /**
   * @param name Special page name.
   * @return Special page.
   */
  public SpecialPage getSpecialPageByName(String name) {
    if ((name == null) || (specialPages == null)) {
      return null;
    }
    return specialPages.get(name); 
  }

  /**
   * @param value Special page alias.
   * @return Special page.
   */
  public SpecialPage getSpecialPageByAlias(String value) {
    if ((value == null) || (specialPages == null)) {
      return null;
    }
    for (SpecialPage specialPage : specialPages.values()) {
      if (specialPage.isPossibleAlias(value)) {
        return specialPage;
      }
    }
    return null; 
  }

  /**
   * @param specialPages Special pages.
   */
  public void setSpecialPages(Map<String, SpecialPage> specialPages) {
    this.specialPages = specialPages;
  }

  // ==========================================================================
  // Linter categories
  // ==========================================================================

  /**
   * Linter categories.
   */
  private List<LinterCategory> linterCategories;

  /**
   * @return Linter categories.
   */
  public List<LinterCategory> getLinterCategories() {
    return linterCategories;
  }

  /**
   * @param linterCategories Linter categories.
   */
  public void setLinterCategories(List<LinterCategory> linterCategories) {
    this.linterCategories = linterCategories;
  }

  // ==========================================================================
  // Messages
  // ==========================================================================

  /**
   * Messages.
   */
  private Map<String, String> messages;

  /**
   * @param name Message name.
   * @return Message.
   */
  public String getMessageByName(String name) {
    if ((name == null) || (messages == null)) {
      return null;
    }
    return messages.get(name); 
  }

  /**
   * @param messages Messages.
   */
  public void setMessages(Map<String, String> messages) {
    this.messages = messages;
  }
}
