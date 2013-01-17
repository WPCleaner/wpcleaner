/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.api.data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.zip.GZIPInputStream;


/**
 * A registry for languages.
 */
public class LanguageRegistry {

  /**
   * Lock object for initialization.
   */
  private final static Object lock = new Object();

  /**
   * List of languages.
   */
  private static List<LanguageRegistry.Language> languages;

  /**
   * Constructor.
   */
  public LanguageRegistry() {
    try {
      initializeRegistry();
    } catch (IOException e) {
      // TODO
    }
  }

  /**
   * @return List of all languages.
   */
  public List<LanguageRegistry.Language> getLanguages() {
    return languages;
  }
  
  /**
   * Initialize registry.
   * @throws IOException 
   */
  private static void initializeRegistry() throws IOException {
    synchronized (lock) {
      if (languages == null) {
        // Open file containing the registry
        URL url = LanguageRegistry.class.getClassLoader().getResource(
            "org/wikipediacleaner/api/data/language-subtag-registry.txt.gz");
        if (url == null) {
          return;
        }
        BufferedReader reader = new BufferedReader(new InputStreamReader(
            new GZIPInputStream(url.openStream())));

        List<LanguageRegistry.Language> tmpLanguages = new ArrayList<LanguageRegistry.Language>();
        String line = null;
        while ((line = reader.readLine()) != null) {
          List<String> lines = new ArrayList<String>();
          while ((line != null) && (!"%%".equals(line.trim()))) {
            lines.add(line);
            line = reader.readLine();
          }
          if ("Type".equals(getElementName(0, lines))) {
            String type = getElementValue(0, lines);
            if ("language".equals(type)) {
              if ("Subtag".equals(getElementName(1, lines))) {
                Language language = new Language(getElementValue(1, lines));
                tmpLanguages.add(language);
              }
            }
          }
        }
        Collections.sort(tmpLanguages);
        languages = Collections.unmodifiableList(tmpLanguages);
      }
    }
  }

  /**
   * @param lineNum Current line.
   * @param lines Array of lines.
   * @return Element name if the line is defining an element.
   */
  private static String getElementName(int lineNum, List<String> lines) {
    if ((lines == null) || (lineNum >= lines.size())) {
      return null;
    }
    String line = lines.get(lineNum);
    if (line.startsWith(" ")) {
      return null;
    }
    int colonIndex = line.indexOf(':');
    if (colonIndex <= 0) {
      return null;
    }
    return line.substring(0, colonIndex);
  }

  /**
   * @param lineNum Current line.
   * @param lines Array of lines.
   * @return Element value if the line is defining an element.
   */
  private static String getElementValue(int lineNum, List<String> lines) {
    if ((lines == null) || (lineNum >= lines.size())) {
      return null;
    }
    String line = lines.get(lineNum);
    if (line.startsWith(" ")) {
      return null;
    }
    int colonIndex = line.indexOf(':');
    if (colonIndex <= 0) {
      return null;
    }
    String value = line.substring(colonIndex + 1).trim();
    // TODO: Manage values on several lines
    return value;
  }

  /**
   * Bean for holding information about a language.
   */
  public static class Language implements Comparable<Language> {

    /**
     * Language code.
     */
    private final String code;
    
    /**
     * @param code Language code.
     */
    public Language(String code) {
      this.code = code;
    }

    /**
     * @return Language code.
     */
    public String getCode() {
      return code;
    }

    /**
     * @param o Other language.
     * @return
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(Language o) {
      return code.compareTo(o.code);
    }
  }
}
