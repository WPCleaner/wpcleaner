/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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


/**
 * Information about inter-wiki.
 */
public class Interwiki implements Comparable<Interwiki> {

  private final String prefix;
  private final boolean local;
  private final String language;
  private final String url;

  /**
   * @param prefix Inter-wiki prefix.
   * @param local Inter-wiki local.
   * @param language Inter-wiki language.
   * @param url Inter-wiki URL.
   */
  public Interwiki(String prefix, boolean local, String language, String url) {
    this.prefix = prefix;
    this.local = local;
    this.language = language;
    this.url = url;
  }

  /**
   * @return Inter-wiki prefix.
   */
  public String getPrefix() {
    return prefix;
  }

  /**
   * @return Inter-wiki local.
   */
  public boolean getLocal() {
    return local;
  }

  /**
   * @return Inter-wiki language.
   */
  public String getLanguage() {
    return language;
  }

  /**
   * @return Inter-wiki URL.
   */
  public String getURL() {
    return url;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(Interwiki iw) {
    int compare;

    // Prefix
    compare = prefix.compareTo(iw.prefix);
    if (compare != 0) {
      return compare;
    }

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
    Interwiki iw = (Interwiki) o;
    boolean equals = true;
    equals &= prefix.equals(iw.prefix);
    equals &= (local == iw.local);
    equals &= (language == null) ? (iw.language == null) : language.equals(iw.language);
    equals &= (url == null) ? (iw.url == null) : url.equals(iw.url);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hash = 7;
    hash = 31 * hash + prefix.hashCode();
    return hash;
  }
}
