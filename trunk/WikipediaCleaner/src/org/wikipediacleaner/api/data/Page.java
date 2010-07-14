/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Informations about pages
 */
public class Page implements Comparable<Page> {

  private EnumWikipedia wikipedia;
  private Integer pageId;
  private Integer namespace;
  private String  title;
  private String  contents;
  private Integer revisionId;
  private String  contentsTimestamp;
  private String  startTimestamp;
  private String  editToken;
  private Boolean disambiguation;
  private Boolean wiktionaryLink;
  private Boolean exist;

  private ArrayList<Page> links;
  private ArrayList<Page> backLinks;
  private ArrayList<Page> embeddedIn;
  private ArrayList<Page> templates;
  private ArrayList<Page> redirects;
  private int countOccurence;

  private PageComment comment;

  private ProgressionValue backLinksProgression;
  private ProgressionValue backLinksMainProgression;
  private ProgressionValue backLinksTemplateProgression;

  /**
   * @param wikipedia Wikipedia;
   * @param title Page title.
   */
  Page(EnumWikipedia wikipedia, String title) {
    this.wikipedia = wikipedia;
    this.title = title;
  }

  /**
   * @param title1 Title 1.
   * @param title2 Title 2.
   * @return Indicates if <code>title1</code> and <code>title2</code> are the same title.
   */
  public static boolean areSameTitle(String title1, String title2) {
    if ((title1 == null) || (title2 == null)) {
      return false;
    }
    if ((title1.length() == 0) || (title2.length() == 0)) {
      return false;
    }
    if (Character.toUpperCase(title1.charAt(0)) != Character.toUpperCase(title2.charAt(0))) {
      return false;
    }
    return title1.substring(1).equals(title2.substring(1));
  }

  /**
   * @return Page id.
   */
  public Integer getPageId() {
    return pageId;
  }

  /**
   * @param pageId Page id. 
   */
  public void setPageId(String pageId) {
    try {
      this.pageId = new Integer(pageId);
    } catch (NumberFormatException e) {
      this.pageId = Integer.valueOf(-1);
    }
  }

  /**
   * @return Namespace number.
   */
  public Integer getNamespace() {
    return namespace;
  }

  /**
   * @param namespace Namespace number.
   */
  public void setNamespace(String namespace) {
    try {
      this.namespace = new Integer(namespace);
    } catch (NumberFormatException e) {
      this.namespace = null;
    }
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @return Title.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @param text Original text.
   * @return Same text with the first letter in upper case.
   */
  public static String getStringUcFirst(String text) {
    if ((text != null) && (text.length() > 0) && (Character.isLowerCase(text.charAt(0)))) {
      return "" + Character.toUpperCase(text.charAt(0)) + text.substring(1);
    }
    return text;
  }

  /**
   * @return Title with first letter as uppercase
   */
  public String getTitleUcFirst() {
    return getStringUcFirst(title);
  }

  /**
   * @return Title with first letter as lowercase
   */
  public String getTitleLcFirst() {
    if ((title != null) && (title.length() > 0) && (Character.isUpperCase(title.charAt(0)))) {
      return "" + Character.toLowerCase(title.charAt(0)) + title.substring(1);
    }
    return title;
  }

  /**
   * @param title Title.
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * @return Page contents.
   */
  public String getContents() {
    return contents;
  }

  /**
   * @param contents Page contents.
   */
  public void setContents(String contents) {
    this.contents = contents;
  }

  /**
   * @return Revision id.
   */
  public Integer getRevisionId() {
    return revisionId;
  }

  /**
   * @param revisionId Revision id. 
   */
  public void setRevisionId(String revisionId) {
    this.revisionId = Integer.valueOf(-1);
    if (revisionId == null) {
      return;
    }
    while (revisionId.startsWith("\"")) {
      revisionId = revisionId.substring(1);
    }
    while (revisionId.endsWith("\"")) {
      revisionId = revisionId.substring(0, revisionId.length() - 1);
    }
    try {
      this.revisionId = new Integer(revisionId);
    } catch (NumberFormatException e) {
      //
    }
  }

  /**
   * @return Contents timestamp.
   */
  public String getContentsTimestamp() {
    return contentsTimestamp;
  }

  /**
   * @param timestamp Contents timestamp.
   */
  public void setContentsTimestamp(String timestamp) {
    this.contentsTimestamp = timestamp;
  }

  /**
   * @return Age of the contents compared to the start date (in seconds).
   */
  public Long getContentsAge() {
    if ((contentsTimestamp == null) || (startTimestamp == null)) {
      return null;
    }
    SimpleDateFormat format = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'Z'");
    try {
      Date dateContents = format.parse(contentsTimestamp);
      Date dateStart = format.parse(startTimestamp);
      long duration = (dateStart.getTime() - dateContents.getTime()) / 1000;
      return Long.valueOf(duration);
    } catch (ParseException e) {
      //
    }
    return null;
  }

  /**
   * @return Start timestamp.
   */
  public String getStartTimestamp() {
    return startTimestamp;
  }

  /**
   * @param timestamp Start timestamp.
   */
  public void setStartTimestamp(String timestamp) {
    this.startTimestamp = timestamp;
  }

  /**
   * @return Edit token.
   */
  public String getEditToken() {
    return editToken;
  }

  /**
   * @param token Edit token.
   */
  public void setEditToken(String token) {
    this.editToken = token;
  }

  /**
   * @return Flag indicating if this is a disambiguation page.
   *         (null means unknown).
   */
  public Boolean isDisambiguationPage() {
    if (Boolean.TRUE.equals(disambiguation)) {
      return Boolean.TRUE;
    }
    if (redirects != null) {
      for (int i = 0; i < redirects.size(); i++) {
        if (Boolean.TRUE.equals(redirects.get(i).isDisambiguationPage())) {
          return Boolean.TRUE;
        }
      }
    }
    return disambiguation;
  }

  /**
   * @param disambiguation Disambiguation page.
   */
  public void setDisambiguationPage(Boolean disambiguation) {
    this.disambiguation = disambiguation;
  }

  /**
   * @return Flag indicating if page has a link to Wiktionary.
   */
  public boolean hasWiktionaryTemplate() {
    if (wiktionaryLink != null) {
      return wiktionaryLink.booleanValue();
    }
    if ((templates == null) ||
        (wikipedia == null) ||
        (wikipedia.getWiktionaryMatchesCount() == 0)){
      return false;
    }
    for (Page template : templates) {
      for (int i = 0; i < wikipedia.getWiktionaryMatchesCount(); i++) {
        TemplateMatch match = wikipedia.getWiktionaryMatch(i);
        if (areSameTitle(match.getName(), template.getTitle())) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @return Flag indicating if page has a link to Wiktionary.
   */
  public Boolean hasWiktionaryLink() {
    return wiktionaryLink;
  }

  /**
   * @param wiktionary Flag indicating if page has a link to Wiktionary.
   */
  public void setWiktionaryLink(Boolean wiktionary) {
    this.wiktionaryLink = wiktionary;
  }

  /**
   * @return Flag indicating if the page exists.
   *         (null means unknow).
   */
  public Boolean isExisting() {
    return exist;
  }

  /**
   * @param exist Flag indicating if the page exists.
   */
  public void setExisting(Boolean exist) {
    this.exist = exist;
  }

  /**
   * @return Flag indicating if there's a redirection
   */
  public boolean isRedirect() {
    return redirects != null;
  }

  /**
   * @return The title of the page when redirects are followed.
   */
  public String getRedirectTitle() {
    if ((redirects != null) && (redirects.size() > 0)) {
      return redirects.get(redirects.size() - 1).getTitle();
    }
    return getTitle();
  }

  /**
   * @return Redirection.
   */
  public ArrayList<Page> getRedirects() {
    return redirects;
  }

  /**
   * @return Redirection destination.
   */
  public String getRedirectDestination() {
    if ((redirects == null) || (redirects.isEmpty())) {
      return getTitle();
    }
    Page to = redirects.get(redirects.size() - 1);
    String toTitle = to.getTitle();
    String pageContents = contents;
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
          final String r1 = "#REDIRECTION";
          final String r2 = "#REDIRECT";
          if (r1.equalsIgnoreCase(pageContents.substring(startIndex, startIndex + r1.length()))) {
            startIndex += r1.length();
          } else if (r2.equalsIgnoreCase(pageContents.substring(startIndex, startIndex + r2.length()))) {
            startIndex += r2.length();
          } else {
            ok = false;
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
    if ((to.getNamespace() != null) &&
        (to.getNamespace() == Namespace.CATEGORY) &&
        (!toTitle.startsWith(":"))) {
      toTitle = ":" + toTitle;
    }
    return toTitle;
  }
  
  /**
   * @param redirect Redirection.
   */
  public void setRedirects(ArrayList<Page> redirect) {
    this.redirects = redirect;
  }

  /**
   * @param redirect Redirection.
   */
  public void addRedirect(Page redirect) {
    if (redirects == null) {
      redirects = new ArrayList<Page>(); 
    }
    redirects.add(redirect);
  }

  /**
   * @return Flag indicating if the page is an article (not a talk page).
   */
  public boolean isArticle() {
    return (namespace != null) && (namespace.intValue() % 2 == 0);
  }

  /**
   * @return Flag indicating if the page is in the main namespace.
   */
  public boolean isInMainNamespace() {
    return (namespace != null) && (namespace.intValue() == Namespace.MAIN);
  }

  /**
   * @return Flag indicating if the page is in the template namespace.
   */
  public boolean isInTemplateNamespace() {
    return (namespace != null) && (namespace.intValue() == Namespace.TEMPLATE);
  }

  /**
   * @param namespaces List of namespaces.
   * @return Talk page title.
   */
  public String getTalkPage(List<Namespace> namespaces) {
    if (!isArticle() || (namespace == null) || (namespaces == null)) {
      return null;
    }
    if (Namespace.MAIN == namespace.intValue()) {
      return "Talk:" + title;
    }
    Namespace n = Namespace.getNamespace(namespace.intValue() + 1, namespaces);
    int firstColon = title.indexOf(':');
    if ((firstColon >= 0) && (n != null)) {
      return n.getTitle() + ":" + title.substring(firstColon + 1);
    }
    return null;
  }

  /**
   * @return Links from the page.
   */
  public ArrayList<Page> getLinks() {
    return links;
  }

  /**
   * @return Links from the page (working if the page is a redirection).
   */
  public ArrayList<Page> getLinksWithRedirect() {
    if (redirects != null) {
      for (int i = 0; i < redirects.size(); i++) {
        ArrayList<Page> redirectLinks = redirects.get(i).links;
        if ((redirectLinks != null) && (!redirectLinks.isEmpty())) {
          return redirectLinks;
        }
      }
    }
    return links;
  }

  /**
   * @param anchors Anchors among the link (OUT).
   * @return Links from the page (working if the page is a redirection).
   */
  public ArrayList<Page> getLinksWithRedirect(HashMap<Page, List<String>> anchors) {
    if (redirects != null) {
      for (int i = 0; i < redirects.size(); i++) {
        Page page = redirects.get(i);
        ArrayList<Page> redirectLinks = page.links;
        if ((redirectLinks != null) && (!redirectLinks.isEmpty())) {
          getAnchors(page.getContents(), redirectLinks, anchors);
          return redirectLinks;
        }
      }
    }
    if (links != null) {
      getAnchors(contents, links, anchors);
    } else if (redirects != null) {
      getAnchors(contents, redirects, anchors);
    }
    return links;
  }

  /**
   * @param pageContents Page contents.
   * @param pageLinks Page links.
   * @param anchors Anchors (OUT)
   */
  private void getAnchors(String pageContents, ArrayList<Page> pageLinks, HashMap<Page, List<String>> anchors) {
    if ((pageContents != null) && (pageContents.length() > 0) && (anchors != null)) {
      int currentPos = 0;
      int nextSharp = 0;
      // Check each internal link
      while ((currentPos >= 0) && (currentPos < pageContents.length())) {
        currentPos = pageContents.indexOf("[[", currentPos);
        if (currentPos > 0) {
          int tmpPos = currentPos + 2;
          // Check the next anchor
          if (nextSharp < currentPos) {
            nextSharp = pageContents.indexOf("#", tmpPos);
          }
          if (nextSharp < 0) {
            currentPos = -1;
          } else {
            // Check the next end of link 
            int nextEnd = pageContents.indexOf("]]", tmpPos);
            if (nextEnd < 0) {
              currentPos = -1;
            } else {
              // If the anchor is not for the link, go to the next link
              if (nextSharp > nextEnd) {
                currentPos = nextEnd;
              } else {
                // Possible white spaces
                while ((tmpPos < pageContents.length()) && (pageContents.charAt(tmpPos) == ' ')) {
                  tmpPos++;
                }
                // Check each link
                for (Page link : pageLinks) {
                  int currentIndex = 0;
                  boolean equals = true;
                  String linkTitle = link.getTitle();
                  while (equals && (currentIndex < linkTitle.length()) && (tmpPos + currentIndex < nextSharp)) {
                    char contentsChar = contents.charAt(tmpPos + currentIndex);
                    char linkTitleChar = linkTitle.charAt(currentIndex);
                    if (currentIndex == 0) {
                      if (Character.toUpperCase(linkTitleChar) != Character.toUpperCase(contentsChar)) {
                        equals = false;
                      }
                    } else if (linkTitleChar == ' ') {
                      if ((contentsChar != ' ') && (contentsChar != '_')) {
                        equals = false;
                      }
                    } else {
                      if (linkTitleChar != contentsChar) {
                        equals = false;
                      }
                    }
                    currentIndex++;
                  }
                  // Possible white spaces
                  while ((tmpPos + currentIndex < pageContents.length()) &&
                         (pageContents.charAt(currentPos + currentIndex) == ' ')) {
                    currentIndex++;
                  }
                  if (tmpPos + currentIndex == nextSharp) {
                    int nextPipe = contents.indexOf("|", nextSharp);
                    String anchor;
                    if ((nextPipe >= 0) && (nextPipe < nextEnd)) {
                      anchor = contents.substring(tmpPos, nextPipe);
                    } else {
                      anchor = contents.substring(tmpPos, nextEnd);
                    }
                    if ((anchor != null) && (anchor.trim().length() > 0)) {
                      List<String> listAnchors = anchors.get(link);
                      if (listAnchors == null) {
                        listAnchors = new ArrayList<String>();
                        anchors.put(link, listAnchors);
                      }
                      if (!listAnchors.contains(anchor)) {
                        listAnchors.add(anchor);
                      }
                    }
                  }
                }
                currentPos++;
              }
            }
          }
        }
      }
    }
  }

  /**
   * @return Iterator for the page + redirects
   */
  public Iterator<Page> getRedirectIteratorWithPage() {
    ArrayList<Page> list = new ArrayList<Page>(
        (redirects != null) ? redirects.size() + 1 : 1);
    list.add(this);
    if (redirects != null) {
      list.addAll(redirects);
    }
    return list.iterator();
  }

  /**
   * @param links Links from the page.
   */
  public void setLinks(ArrayList<Page> links) {
    this.links = links;
    if (this.links != null) {
      Collections.sort(this.links);
    }
  }

  /**
   * @return Back links of the page.
   */
  public ArrayList<Page> getBackLinks() {
    return backLinks;
  }

  /**
   * @param links Back links of the page.
   */
  public void setBackLinks(ArrayList<Page> links) {
    this.backLinks = links;
  }

  /**
   * @return Back links of the page (including redirects).
   */
  public ArrayList<Page> getBackLinksWithRedirects() {
    ArrayList<Page> result = backLinks;
    boolean originalList = true;
    if (backLinks != null) {
      for (Page p : backLinks) {
        if (p.isRedirect()) {
          ArrayList<Page> tmpRedirects = p.getRedirects();
          for (int i = 0; i < tmpRedirects.size(); i++) {
            Page tmp = tmpRedirects.get(i);
            if (areSameTitle(title, tmp.getTitle())) {
              ArrayList<Page> tmpBackLinks = p.getBackLinksWithRedirects();
              if ((tmpBackLinks != null) && (!tmpBackLinks.isEmpty())) {
                if (originalList) {
                  result = new ArrayList<Page>(result);
                  originalList = false;
                }
                result.addAll(tmpBackLinks);
              }
              break;
            }
          }
        }
      }
    }
    if (!originalList) {
      Collections.sort(result);
    }
    if (result != null) {
      for (int i = 0; i < result.size() - 1; i++) {
        if (areSameTitle(result.get(i).getTitle(), result.get(i + 1).getTitle())) {
          result.remove(i + 1);
        }
      }
    }
    return result;
  }

  /**
   * @return Backlinks count.
   */
  public Integer getBacklinksCount() {
    ArrayList<Page> backlinks = getBackLinksWithRedirects();
    if (backlinks != null) {
      return backlinks.size();
    }
    return null;
  }

  /**
   * @return A simple object indicating progression.
   */
  public ProgressionValue getBacklinksProgression() {
    if (backLinksProgression == null) {
      backLinksProgression = new ProgressionValue(null, null);
    }
    backLinksProgression.setCurrent(getBacklinksCount());
    if (comment != null) {
      backLinksProgression.setGoal(comment.getMaxArticles());
    } else {
      backLinksProgression.setGoal(null);
    }
    return backLinksProgression;
  }

  /**
   * @return Backlinks count in article namespace.
   */
  public Integer getBacklinksCountInMainNamespace() {
    ArrayList<Page> backlinks = getBackLinksWithRedirects();
    if (backlinks != null) {
      int count = 0;
      for (int i = 0; i < backlinks.size(); i++) {
        if (backlinks.get(i).isInMainNamespace()) {
          count++;
        }
      }
      return Integer.valueOf(count);
    }
    return null;
  }

  /**
   * @return A simple object indicating progression.
   */
  public ProgressionValue getBacklinksProgressionInMainNamespace() {
    if (backLinksMainProgression == null) {
      backLinksMainProgression = new ProgressionValue(null, null);
    }
    backLinksMainProgression.setCurrent(getBacklinksCountInMainNamespace());
    if (comment != null) {
      backLinksMainProgression.setGoal(comment.getMaxMainArticles());
    } else {
      backLinksMainProgression.setGoal(null);
    }
    return backLinksMainProgression;
  }

  /**
   * @return Backlinks count in template namespace.
   */
  public Integer getBacklinksCountInTemplateNamespace() {
    ArrayList<Page> backlinks = getBackLinksWithRedirects();
    if (backlinks != null) {
      int count = 0;
      for (int i = 0; i < backlinks.size(); i++) {
        if (backlinks.get(i).isInTemplateNamespace()) {
          count++;
        }
      }
      return Integer.valueOf(count);
    }
    return null;
  }

  /**
   * @return A simple object indicating progression.
   */
  public ProgressionValue getBacklinksProgressionInTemplateNamespace() {
    if (backLinksTemplateProgression == null) {
      backLinksTemplateProgression = new ProgressionValue(null, null);
    }
    backLinksTemplateProgression.setCurrent(getBacklinksCountInTemplateNamespace());
    if (comment != null) {
      backLinksTemplateProgression.setGoal(comment.getMaxTemplateArticles());
    } else {
      backLinksTemplateProgression.setGoal(null);
    }
    return backLinksTemplateProgression;
  }

  /**
   * @return Pages embedding this page.
   */
  public ArrayList<Page> getEmbeddedIn() {
    return embeddedIn;
  }

  /**
   * @param pages Pages embedding this page.
   */
  public void setEmbeddedIn(ArrayList<Page> pages) {
    this.embeddedIn = pages;
  }

  /**
   * @return Templates of the page.
   */
  public ArrayList<Page> getTemplates() {
    return templates;
  }

  /**
   * @return Links to the wiktionary.
   */
  public ArrayList<String> getWiktionaryLinks() {
    ArrayList<String> wiktionary = null;
    if ((contents != null) && (wikipedia != null)) {
      for (int i = 0; i < wikipedia.getWiktionaryMatchesCount(); i++) {
        TemplateMatch template = wikipedia.getWiktionaryMatch(i);
        Pattern pattern = PageUtilities.createPatternForTemplate(template);
        Matcher matcher = pattern.matcher(contents);
        while (matcher.find()) {
          ArrayList<TemplateParameter> parameters = PageUtilities.analyzeTemplateParameters(template, matcher, this);
          for (TemplateParameter param : parameters) {
            if (param.isRelevant()) {
              if (wiktionary == null) {
                wiktionary = new ArrayList<String>();
              }
              wiktionary.add(param.getValue());
            }
          }
        }
      }
    }
    return wiktionary;
  }

  /**
   * @param templates Templates of the page.
   */
  public void setTemplates(ArrayList<Page> templates) {
    this.templates = templates;
  }

  /**
   * @return Occurence count
   */
  public int getCountOccurence() {
    return countOccurence;
  }

  /**
   * @param count Occurence count
   */
  public void setCountOccurence(int count) {
    this.countOccurence = count;
  }

  /**
   * @return Page comment.
   */
  public PageComment getComment() {
    return comment;
  }

  /**
   * @param comment Page comment.
   */
  public void setComment(PageComment comment) {
    this.comment = comment;
  }

  /**
   * @return Evaluation of {{PAGENAME}}
   */
  public String getMagicPAGENAME() {
    return title;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return title;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(Page bl) {
    int compare;

    // Namespace
    if (namespace == null) {
      if (bl.namespace != null) {
        return -1;
      }
    } else {
      compare = namespace.compareTo(bl.namespace);
      if (compare != 0) {
        return compare;
      }
    }

    // Title
    if (title == null) {
      if (bl.title != null) {
        return -1;
      }
    } else {
      compare = title.compareTo(bl.title);
      if (compare != 0) {
        return compare;
      }
    }

    // Page Id
    if (pageId == null) {
      if (bl.pageId != null) {
        return -1;
      }
    } else {
      compare = pageId.compareTo(bl.pageId);
      if (compare != 0) {
        return compare;
      }
    }
    
    return 0;
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
    Page bl = (Page) o;
    boolean equals = true;
    equals &= (pageId == null) ? (bl.pageId == null) : pageId.equals(bl.pageId);
    equals &= (namespace == null) ? (bl.namespace == null) : namespace.equals(bl.namespace);
    equals &= (title == null) ? (bl.title == null) : title.equals(bl.title);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hash = 7;
    hash = 31 * hash + ((pageId != null) ? pageId.hashCode() : 0);
    hash = 31 * hash + ((namespace != null) ? namespace.hashCode() : 0);
    hash = 31 * hash + ((title != null) ? title.hashCode() : 0);
    return hash;
  }
}
