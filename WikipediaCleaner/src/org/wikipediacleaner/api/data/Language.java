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

import java.util.List;


/**
 * Information about languages.
 */
public class Language implements Comparable<Language> {

  private final String code;
  private final String name;

  /**
   * @param languages List of languages.
   * @param code Language code;
   * @return Language exist ?
   */
  public static boolean isLanguageCode(List<Language> languages, String code) {
    if ((languages == null) || (code == null)) {
      return false;
    }
    for (Language language : languages) {
      if ((language != null) && (code.equals(language.getCode()))) {
        return true;
      }
    }
    return false;
  }

  /**
   * @param code Language code.
   * @param name Language name.
   */
  public Language(String code, String name) {
    this.code = code;
    this.name = name;
  }

  /**
   * @return Language code.
   */
  public String getCode() {
    return code;
  }

  /**
   * @return Language name.
   */
  public String getName() {
    return name;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(Language lg) {
    int compare;

    // Code
    compare = code.compareTo(lg.code);
    if (compare != 0) {
      return compare;
    }

    // Name
    compare = name.compareTo(lg.name);
    
    return compare;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if ((o == null) || (o.getClass() != getClass())) {
      return false;
    }
    Language lg = (Language) o;
    boolean equals = true;
    equals &= code.equals(lg.code);
    equals &= name.equals(lg.name);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hash = 7;
    hash = 31 * hash + code.hashCode();
    hash = 31 * hash + name.hashCode();
    return hash;
  }
}
