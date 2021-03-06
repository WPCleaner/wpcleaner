/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.http.HttpUtils;


/**
 * Information about interwiki.
 */
public class Interwiki implements Comparable<Interwiki> {

  private final String prefix;
  private final boolean local;
  private final String language;
  private final String url;
  private final String urlWithoutProtocol;
  private final String urlWithoutProtocolPrefix;
  private final String urlWithoutProtocolSuffix;

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
    int colonIndex = (url != null) ? url.indexOf(':') : -1;
    this.urlWithoutProtocol = ((colonIndex < 0) || (url == null)) ? null : url.substring(colonIndex + 1);
    int paramIndex = (this.urlWithoutProtocol != null) ? this.urlWithoutProtocol.indexOf("$1") : -1;
    this.urlWithoutProtocolPrefix = (paramIndex > 0) ? this.urlWithoutProtocol.substring(0, paramIndex) : this.urlWithoutProtocol;
    this.urlWithoutProtocolSuffix = (paramIndex > 0) ? this.urlWithoutProtocol.substring(paramIndex + 2) : null;
  }

  /**
   * @param wiki Wiki to test.
   * @return True if the interwiki is for the wiki.
   */
  public boolean isForWiki(EnumWikipedia wiki) {
    if ((wiki.getWikiConfiguration().getServer() != null) &&
        urlWithoutProtocol.startsWith(wiki.getWikiConfiguration().getServer())) {
      return true;
    }
    return false;
  }


  /**
   * Test if an URL matches an article in an interwiki.
   * 
   * @param interwikis List of interwikis.
   * @param test URL to be tested.
   * @return Article if the URL matches an article.
   */
  public static Interwiki isInterwikiUrl(List<Interwiki> interwikis, String test) {
    if (interwikis == null) {
      return null;
    }
    String cleanTest = HttpUtils.getCleanUrlForArticle(test);
    if (cleanTest == null) {
      return null;
    }
    Interwiki result = null;
    for (Interwiki interwiki : interwikis) {
      String tmp = HttpUtils.getArticleFromCleanUrl(
          cleanTest, interwiki.urlWithoutProtocolPrefix, interwiki.urlWithoutProtocolSuffix);
      if (tmp != null) {
        if ((result == null) || (interwiki.getLanguage() != null)) {
          result = interwiki;
        }
      }
    }
    return result;
  }

  /**
   * Test if an URL matches an article.
   * 
   * @param test URL to be tested.
   * @return Article if the URL matches an article.
   */
  public String isArticleUrl(String test) {
    if ((test == null) || (urlWithoutProtocol == null)) {
      return null;
    }

    String result = HttpUtils.getArticleFromUrl(
        test, urlWithoutProtocolPrefix, urlWithoutProtocolSuffix);
    if (result != null) {
      return result;
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
  @Override
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
