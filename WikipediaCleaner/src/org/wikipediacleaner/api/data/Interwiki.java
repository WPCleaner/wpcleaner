/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.HttpUtils;


/**
 * Information about interwiki.
 */
public class Interwiki implements Comparable<Interwiki> {

  private final String prefix;
  private final boolean local;
  private final String language;
  private final String url;

  /**
   * @param prefix Interwiki prefix.
   * @param local Interwiki local.
   * @param language Interwiki language.
   * @param url Interwiki URL.
   */
  public Interwiki(String prefix, boolean local, String language, String url) {
    this.prefix = prefix;
    this.local = local;
    this.language = language;
    this.url = url;
  }

  /**
   * Test if an URL matches an article.
   * 
   * @param test URL to be tested.
   * @return Article if the URL matches an article.
   */
  public String isArticleUrl(String test) {
    if ((test == null) || (url == null)) {
      return null;
    }

    // Check protocol
    if (test.startsWith("http:")) {
      test = test.substring(5);
    } else if (test.startsWith("https:")) {
      test = test.substring(6);
    } else {
      return null;
    }

    // Check URL
    int colonIndex = url.indexOf(':');
    if (colonIndex < 0) {
      return null;
    }
    String tmpUrl = url.substring(colonIndex + 1);
    int paramIndex = tmpUrl.indexOf("$1");
    if (paramIndex >= 0) {
      if (test.startsWith(tmpUrl.substring(0, paramIndex))) {
        String tmp = test.substring(paramIndex);
        String articleName = null;
        if (paramIndex + 2 >= tmpUrl.length()) {
          articleName = tmp;
        } else if (test.endsWith(tmpUrl.substring(paramIndex + 2))) {
          articleName = tmp.substring(0, tmp.length() - tmpUrl.length() + paramIndex + 2);
        }
        articleName = HttpUtils.decodeUrl(articleName);
        return articleName;
      }
    }

    // Check general URL
    final String possibleEnd = "/wiki/$1";
    if (local && tmpUrl.endsWith(possibleEnd)) {
      if ((test.length() == tmpUrl.length() - possibleEnd.length()) ||
          (test.length() == tmpUrl.length() - possibleEnd.length() + 1)) {
        if (tmpUrl.startsWith(test)) {
          return "";
        }
      }
    }

    return null;
  }

  /**
   * @return Interwiki prefix.
   */
  public String getPrefix() {
    return prefix;
  }

  /**
   * @return Interwiki local.
   */
  public boolean getLocal() {
    return local;
  }

  /**
   * @return Interwiki language.
   */
  public String getLanguage() {
    return language;
  }

  /**
   * @return Interwiki URL.
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
