/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.constants;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;


/**
 * Configuration for the wiki.
 */
public class WikiConfiguration {

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
   * @return List of inter-wikis
   */
  public List<Interwiki> getInterwikis() {
    return interwikis;
  }

  /**
   * @param interwikis List of inter-wikis
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
  public MagicWord getMagicWord(String name) {
    if ((name == null) || (magicWords == null)) {
      return null;
    }
    return magicWords.get(name); 
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
      MagicWord magicWord = getMagicWord(functionMagicWord);
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
      MagicWord magicWord = getMagicWord(imgMagicWord);
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
