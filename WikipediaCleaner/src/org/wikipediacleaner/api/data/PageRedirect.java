/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Utility class to handle redirect status and data for a page.
 */
public class PageRedirect {

  /** Page for which redirect status and data are handled */
  private final Page page;

  /** Flag indicating if the page is a redirect */
  private boolean isRedirect;

  /** List of redirects with eventual fragment to link to a specific section of a page */
  private List<Pair<Page, String>> list;

  /** List of redirects */
  private List<Page> pageList;

  /**
   * Default constructor.
   */
  PageRedirect(Page page) {
    this.page = page;
  }

  /**
   * Indicates if the page is a redirection to an other page.
   * 
   * @return <code>true</code> if the page is a redirection.
   */
  public boolean isRedirect() {
    return isRedirect;
  }

  /**
   * Indicates if the page is a redirection to an other page.
   * 
   * @param redirect Indicates if the page is a redirection.
   */
  public void isRedirect(boolean redirect) {
    isRedirect = redirect;
  }

  /**
   * @return Flag indicating if this is a disambiguation page.
   *         (null means unknown).
   */
  public Boolean isDisambiguationPage() {
    if (list == null) {
      return null;
    }
    for (Pair<Page, String> redirect : list) {
      if (Boolean.TRUE.equals(redirect.getLeft().isDisambiguationPage())) {
        return Boolean.TRUE;
      }
    }
    return null;
  }

  /**
   * @return List of redirections.
   */
  public List<Page> getPageList() {
    return pageList;
  }

  /**
   * @return List of redirections.
   */
  public List<Pair<Page, String>> getList() {
    return list;
  }

  /**
   * @return Last page in the list of redirects.
   */
  public Pair<Page, String> getLast() {
    if ((list == null) || (list.isEmpty())) {
      return null;
    }
    return list.get(list.size() - 1);
  }

  /**
   * @return Last page in the list of redirects.
   */
  public Page getLastPage() {
    Pair<Page, String> redirect = getLast();
    return (redirect != null) ? redirect.getLeft() : null;
  }

  /**
   * Set the list of redirections.
   * 
   * @param pages List of redirections.
   */
  public void setPageList(List<Page> pages) {
    if (pages == null) {
      list = null;
      pageList = null;
      return;
    }
    list = new ArrayList<>();
    pageList = new ArrayList<>();
    for (Page tmpPage : pages) {
      list.add(new ImmutablePair<>(tmpPage, (String) null));
      pageList.add(tmpPage);
    }
    isRedirect = true;
  }

  /**
   * Add a redirect to the list of redirects.
   * 
   * @param redirect Redirection.
   * @param fragment Eventual fragment to go to a specific section in the redirection.
   */
  public void add(Page redirect, String fragment) {
    if (list == null) {
      list = new ArrayList<>();
      pageList = new ArrayList<>();
    }
    list.add(new ImmutablePair<>(redirect, fragment));
    pageList.add(redirect);
    isRedirect = true;
  }

  /**
   * @return The title of the page when redirects are followed.
   */
  public String getTitle() {
    Page lastPage = getLastPage();
    if (lastPage != null) {
      return lastPage.getTitle();
    }
    return page.getTitle();
  }

  /**
   * @return Redirection destination.
   */
  public String getDestination() {
    if ((list == null) || (list.isEmpty())) {
      return getTitle();
    }
    Pair<Page, String> to = getLast();
    Page toPage = to.getLeft();
    String toTitle = toPage.getTitle();
    if (to.getRight() != null) {
      return toTitle + "#" + to.getRight();
    }
    String pageContents = page.getContents();
    if ((pageContents != null) && (pageContents.length() > 0)) {
      boolean redirectFound = false;
      int startIndex = 0;
      while ((!redirectFound) && (startIndex < pageContents.length())) {
        boolean ok = true;
        int endIndex = pageContents.indexOf('\n', startIndex);
        if (endIndex < 0) {
          endIndex = pageContents.length();
        }
        // Removing white spaces
        if (ok) {
          while ((startIndex < endIndex) &&
                 Character.isWhitespace(pageContents.charAt(startIndex))) {
            startIndex++;
          }
        }
        // Removing REDIRECT
        if (ok) {
          ok = false;
          MagicWord magicRedirect = page.getWikipedia().getWikiConfiguration().getMagicWordByName(MagicWord.REDIRECT);
          if ((magicRedirect != null) && (magicRedirect.getAliases() != null)) {
            int length = 0;
            for (String magic : magicRedirect.getAliases()) {
              if ((pageContents.length() > startIndex + magic.length()) &&
                  (magic.length() > length)) {
                if (magic.equalsIgnoreCase(pageContents.substring(startIndex, startIndex + magic.length()))) {
                  ok = true;
                  length = magic.length();
                }
              }
            }
            startIndex += length;
          }
        }
        // Removing white spaces
        if (ok) {
          while ((startIndex < endIndex) &&
                 Character.isWhitespace(pageContents.charAt(startIndex))) {
            startIndex++;
          }
        }
        if (ok && pageContents.startsWith("[[", startIndex)) {
          redirectFound = true;
          startIndex += "[[".length();
          int endRedirect = pageContents.indexOf("]]", startIndex);
          int sharp = pageContents.indexOf("#", startIndex);
          if ((endRedirect > 0) && (sharp > 0) && (sharp < endRedirect)) {
            toTitle += pageContents.substring(sharp, endRedirect);
          }
        }
        startIndex = endIndex + 1;
      }
    }
    if ((toPage.getNamespace() != null) &&
        (toPage.getNamespace() == Namespace.CATEGORY) &&
        (!toTitle.startsWith(":"))) {
      toTitle = ":" + toTitle;
    }
    return toTitle;
  }

  /**
   * @return Links from the page (considering the redirects).
   */
  public List<Page> getLinks() {
    if (list != null) {
      for (Pair<Page, String> redirect : list) {
        List<Page> redirectLinks = redirect.getLeft().getLinks();
        if ((redirectLinks != null) && (!redirectLinks.isEmpty())) {
          return redirectLinks;
        }
      }
    }
    return page.getLinks();
  }

  /**
   * @param anchors Anchors among the link (OUT).
   * @return Links from the page (considering the redirection).
   */
  public List<Page> getLinks(Map<Page, List<String>> anchors) {
    PageAnalysis pageAnalysis = page.getAnalysis(page.getContents(), false);
    if (list != null) {
      for (Pair<Page, String> redirect : list) {
        Page tmpPage = redirect.getLeft();
        List<Page> redirectLinks = tmpPage.getLinks();
        if ((redirectLinks != null) && (!redirectLinks.isEmpty())) {
          PageAnalysisUtils.getAnchors(pageAnalysis, redirectLinks, anchors);
          return redirectLinks;
        }
      }
    }
    if (page.getLinks() != null) {
      PageAnalysisUtils.getAnchors(pageAnalysis, page.getLinks(), anchors);
    } else if (pageList != null) {
      PageAnalysisUtils.getAnchors(pageAnalysis, pageList, anchors);
    }
    return page.getLinks();
  }

  /**
   * @return Iterator for the page + redirects
   */
  public Iterator<Page> getIteratorWithPage() {
    List<Page> tmpList = new ArrayList<>((list != null) ? list.size() + 1 : 1);
    tmpList.add(page);
    if (list != null) {
      for (Pair<Page, String> redirect : list) {
        tmpList.add(redirect.getLeft());
      }
    }
    return tmpList.iterator();
  }

}
