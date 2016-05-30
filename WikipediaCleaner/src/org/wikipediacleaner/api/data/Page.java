/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WikiConfiguration;


/**
 * Information about pages.
 */
public class Page implements Comparable<Page> {

  /**
   * Kinds of related pages.
   */
  public static enum RelatedPages {
    @Deprecated BACKLINKS, // Replaced by LINKS_HERE (back links notion is too problematic in API)
    CATEGORIES,
    CATEGORY_MEMBERS,
    EMBEDDED_IN,
    LINKS_HERE,
    REDIRECTS,
    SIMILAR_PAGES;
  }

  private EnumWikipedia wikipedia;
  private Integer pageId;
  private Integer namespace;
  private String  title;
  private String  contents;
  private Integer revisionId;
  private String  contentsTimestamp;
  private String  startTimestamp;
  private String  editProtectionLevel;
  private Boolean disambiguation;
  private Boolean wiktionaryLink;
  private Boolean exist;

  private List<Page> links;
  private List<Page> templates;

  private final Map<RelatedPages, List<Page>> relatedPages;

  private PageComment comment;

  private ProgressionValue backLinksMainProgression;
  private ProgressionValue backLinksOtherProgression;
  private ProgressionValue backLinksTemplateProgression;

  /**
   * @param wiki Wiki;
   * @param title Page title.
   */
  Page(EnumWikipedia wikipedia, String title) {
    this.wikipedia = wikipedia;
    this.title = title;
    this.relatedPages = new Hashtable<Page.RelatedPages, List<Page>>();
    setContents("");
  }

  /**
   * @return Page replicated.
   */
  public Page replicatePage() {
    Page page = new Page(wikipedia, title);
    page.pageId = pageId;
    page.namespace = namespace;
    page.revisionId = revisionId;
    return page;
  }

  /**
   * @param title1 Title 1.
   * @param title2 Title 2.
   * @return Indicates if <code>title1</code> and <code>title2</code> are the same title.
   */
  public static boolean areSameTitle(String title1, String title2) {
    // TODO: should be by Wiki (capitalization of first letter)
    if ((title1 == null) || (title2 == null)) {
      return false;
    }
    title1 = normalizeTitle(title1);
    title2 = normalizeTitle(title2);
    return title1.equals(title2);
  }

  /**
   * @param pageTitle Title.
   * @return Normalized title.
   */
  public static String normalizeTitle(String pageTitle) {
    // TODO: should be by Wiki (capitalization of first letter)
    if (pageTitle == null) {
      return null;
    }
    String result = pageTitle.trim();
    result = result.replaceAll(" ", " ");
    while ((result.length() > 0) && (result.charAt(result.length() - 1) == 0x200E)) {
      result = result.substring(0, result.length() - 1);
    }
    result = result.replaceAll("_", " ");
    result = result.replaceAll(" +", " ");
    result = result.trim();
    result = Page.getStringUcFirst(result);
    return result;
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
  public void setPageId(Integer pageId) {
    this.pageId = pageId;
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
   * @return Name space number.
   */
  public Integer getNamespace() {
    return namespace;
  }

  /**
   * @param namespace Name space number.
   */
  public void setNamespace(String namespace) {
    try {
      this.namespace = new Integer(namespace);
    } catch (NumberFormatException e) {
      this.namespace = null;
    }
  }

  /**
   * @param namespace Name space number.
   */
  public void setNamespace(Integer namespace) {
    this.namespace = namespace;
  }

  /**
   * @return Wiki.
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
   * @return Value for {{PAGENAME}} magic word.
   */
  public String getValuePAGENAME() {
    if ((title == null) || (namespace == null) || (Namespace.MAIN == namespace.intValue())) {
      return title;
    }
    int colonIndex = title.indexOf(':');
    if (colonIndex >= 0) {
      return title.substring(colonIndex + 1);
    }
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
   * @return Title with first letter as upper case
   */
  public String getTitleUcFirst() {
    return getStringUcFirst(title);
  }

  /**
   * @return Title with first letter as lower case
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
   * @return Contents time stamp.
   */
  public String getContentsTimestamp() {
    return contentsTimestamp;
  }

  /**
   * @param timestamp Contents time stamp.
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
    try {
      Date dateContents = DataManager.convertIso8601DateTime(contentsTimestamp);
      Date dateStart = DataManager.convertIso8601DateTime(startTimestamp);
      long duration = (dateStart.getTime() - dateContents.getTime()) / 1000;
      return Long.valueOf(duration);
    } catch (ParseException e) {
      //
    }
    return null;
  }

  /**
   * @return Start time stamp.
   */
  public String getStartTimestamp() {
    return startTimestamp;
  }

  /**
   * @param timestamp Start time stamp.
   */
  public void setStartTimestamp(String timestamp) {
    this.startTimestamp = timestamp;
  }

  /**
   * @return Edit protection level.
   */
  public String getEditProtectionLevel() {
    return editProtectionLevel;
  }

  /**
   * @param level Edit protection level.
   */
  public void setEditProtectionLevel(String level) {
    editProtectionLevel = level;
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
        (wikipedia.getConfiguration().getWiktionaryMatchesCount() == 0)){
      return false;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    for (Page template : templates) {
      for (int i = 0; i < configuration.getWiktionaryMatchesCount(); i++) {
        TemplateMatch match = configuration.getWiktionaryMatch(i);
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
   *         (null means unknown).
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
   * @return Flag indicating if the page is in the user namespace.
   */
  public boolean isInUserNamespace() {
    return (namespace != null) &&
        ((namespace.intValue() == Namespace.USER) || (namespace.intValue() == Namespace.USER_TALK));
  }

  /**
   * @return Article page.
   */
  public Page getArticlePage() {
    if (isArticle()) {
      return this;
    }
    String articlePageName = getArticlePageName();
    if (articlePageName == null) {
      return null;
    }
    Page articlePage = DataManager.getPage(
        getWikipedia(), articlePageName, null, null, null);
    return articlePage;
  }

  /**
   * @return Article page title.
   */
  public String getArticlePageName() {
    if (isArticle()) {
      return title;
    }
    if (namespace == null) {
      return null;
    }
    WikiConfiguration wikiConfiguration = wikipedia.getWikiConfiguration();
    List<Namespace> namespaces = wikiConfiguration.getNamespaces();
    if (namespaces == null) {
      return null;
    }
    if (Namespace.MAIN_TALK == namespace.intValue()) {
      int colonIndex = title.indexOf(':');
      if ((colonIndex >= 0) && (colonIndex + 1 < title.length())) {
        return title.substring(colonIndex + 1);
      }
      return title;
    }
    Namespace n = wikiConfiguration.getNamespace(namespace.intValue() - 1);
    int firstColon = title.indexOf(':');
    if ((firstColon >= 0) && (n != null)) {
      return n.getTitle() + ":" + title.substring(firstColon + 1);
    }
    return null;
  }

  /**
   * @return Talk page.
   */
  public Page getTalkPage() {
    String talkPageName = getTalkPageName();
    if (talkPageName == null) {
      return null;
    }
    Page talkPage = DataManager.getPage(
        getWikipedia(), talkPageName, null, null, null);
    return talkPage;
  }

  /**
   * @return Talk page title.
   */
  public String getTalkPageName() {
    if (!isArticle() ||
        (namespace == null) ||
        (wikipedia == null)) {
      return null;
    }
    WikiConfiguration wikiConfiguration = wikipedia.getWikiConfiguration();
    List<Namespace> namespaces = wikiConfiguration.getNamespaces();
    if (namespaces == null) {
      return null;
    }
    if (Namespace.MAIN == namespace.intValue()) {
      Namespace n = wikiConfiguration.getNamespace(Namespace.MAIN_TALK);
      if (n != null) {
        return n.getTitle() + ":" + title;
      }
      return "Talk:" + title;
    }
    Namespace n = wikiConfiguration.getNamespace(namespace.intValue() + 1);
    int firstColon = title.indexOf(':');
    if ((firstColon >= 0) && (n != null)) {
      return n.getTitle() + ":" + title.substring(firstColon + 1);
    }
    return null;
  }

  /**
   * @param subpage Subpage name.
   * @return Subpage.
   */
  public Page getSubPage(String subpage) {
    Page subPage = DataManager.getPage(
        getWikipedia(), getTitle() + "/" + subpage, null, null, null);
    return subPage;
  }

  /**
   * @param type Type of related pages.
   * @return Related pages.
   */
  public List<Page> getRelatedPages(RelatedPages type) {
    if (relatedPages == null) {
      return null;
    }
    return relatedPages.get(type);
  }

  /**
   * @param pages Similar pages.
   */
  public void setRelatedPages(RelatedPages type, List<Page> pages) {
    if (pages == null) {
      relatedPages.remove(type);
    } else {
      relatedPages.put(type, pages);
    }
  }

  /**
   * @return Links from the page.
   */
  public List<Page> getLinks() {
    return links;
  }

  /**
   * @return Links from the page (working if the page is a redirection).
   */
  public List<Page> getLinksWithRedirect() {
    if (redirects != null) {
      for (int i = 0; i < redirects.size(); i++) {
        List<Page> redirectLinks = redirects.get(i).links;
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
  public List<Page> getLinksWithRedirect(Map<Page, List<String>> anchors) {
    PageAnalysis pageAnalysis = getAnalysis(contents, false);
    if (redirects != null) {
      for (int i = 0; i < redirects.size(); i++) {
        Page page = redirects.get(i);
        List<Page> redirectLinks = page.links;
        if ((redirectLinks != null) && (!redirectLinks.isEmpty())) {
          PageAnalysisUtils.getAnchors(pageAnalysis, redirectLinks, anchors);
          return redirectLinks;
        }
      }
    }
    if (links != null) {
      PageAnalysisUtils.getAnchors(pageAnalysis, links, anchors);
    } else if (redirects != null) {
      PageAnalysisUtils.getAnchors(pageAnalysis, redirects, anchors);
    }
    return links;
  }

  /**
   * @return Iterator for the page + redirects
   */
  public Iterator<Page> getRedirectIteratorWithPage() {
    List<Page> list = new ArrayList<Page>(
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
  public void setLinks(List<Page> links) {
    this.links = links;
    if (this.links != null) {
      Collections.sort(this.links);
    }
  }

  /**
   * @return Links to the page (including through redirects).
   */
  public List<Page> getAllLinksToPage() {
    List<Page> linksHere = getRelatedPages(RelatedPages.LINKS_HERE);
    List<Page> result = linksHere;
    boolean originalList = true;
    List<Page> tmpRedirects = getRelatedPages(RelatedPages.REDIRECTS);
    if (tmpRedirects != null) {
      for (Page p : tmpRedirects) {
        List<Page> tmpLinksHere = p.getAllLinksToPage();
        if ((tmpLinksHere != null) && (!tmpLinksHere.isEmpty())) {
          if (originalList) {
            result = new ArrayList<Page>(result);
            originalList = false;
          }
          result.addAll(tmpLinksHere);
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
    List<Page> backlinks = getAllLinksToPage();
    if (backlinks != null) {
      return backlinks.size();
    }
    return null;
  }

  /**
   * @return Backlinks count in article namespace.
   */
  public Integer getBacklinksCountInMainNamespace() {
    List<Page> backlinks = getAllLinksToPage();
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
   * @return Backlinks count in template namespace.
   */
  public Integer getBacklinksCountInTemplateNamespace() {
    List<Page> backlinks = getAllLinksToPage();
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
  public ProgressionValue getBacklinksProgressionInMainNamespace() {
    if (backLinksMainProgression == null) {
      backLinksMainProgression = new ProgressionValue(null, null, true);
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
   * @return A simple object indicating progression.
   */
  public ProgressionValue getBacklinksProgressionInTemplateNamespace() {
    if (backLinksTemplateProgression == null) {
      backLinksTemplateProgression = new ProgressionValue(null, null, false);
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
   * @return A simple object indicating progression.
   */
  public ProgressionValue getBacklinksProgressionInOtherNamespaces() {
    if (backLinksOtherProgression == null) {
      backLinksOtherProgression = new ProgressionValue(null, null, false);
    }
    Integer current = null;
    if (getBacklinksCount() != null) {
      int tmp = getBacklinksCount().intValue();
      if (getBacklinksCountInMainNamespace() != null) {
        tmp -= getBacklinksCountInMainNamespace().intValue();
      }
      if (getBacklinksCountInTemplateNamespace() != null) {
        tmp -= getBacklinksCountInTemplateNamespace().intValue();
      }
      current = Integer.valueOf(tmp);
    }
    backLinksOtherProgression.setCurrent(current);
    if (comment != null) {
      backLinksOtherProgression.setGoal(comment.getMaxOtherArticles());
    } else {
      backLinksOtherProgression.setGoal(null);
    }
    return backLinksOtherProgression;
  }

  /**
   * @return Templates of the page.
   */
  public List<Page> getTemplates() {
    return templates;
  }

  /**
   * @return Links to the wiktionary.
   */
  public List<String> getWiktionaryLinks() {
    List<String> wiktionary = null;
    if ((contents != null) && (wikipedia != null)) {
      WPCConfiguration configuration = wikipedia.getConfiguration();
      for (int i = 0; i < configuration.getWiktionaryMatchesCount(); i++) {
        TemplateMatch template = configuration.getWiktionaryMatch(i);
        Pattern pattern = PageUtilities.createPatternForTemplate(template);
        Matcher matcher = pattern.matcher(contents);
        while (matcher.find()) {
          List<TemplateParameter> parameters = PageUtilities.analyzeTemplateParameters(template, matcher, this);
          for (TemplateParameter param : parameters) {
            if (param.isRelevant()) {
              if (wiktionary == null) {
                wiktionary = new ArrayList<String>();
              }
              if (!wiktionary.contains(param.getValue())) {
                wiktionary.add(param.getValue());
              }
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
  public void setTemplates(List<Page> templates) {
    this.templates = templates;
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

  // ==========================================================================
  // Redirects
  // ==========================================================================

  /**
   * Flag indicating if the page is a redirect.
   */
  private boolean isRedirect;

  /**
   * List of pages the page is redirecting to.
   */
  private List<Page> redirects;

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
   * @return Redirection.
   */
  public List<Page> getRedirects() {
    return redirects;
  }
  
  /**
   * @param redirect Redirection.
   */
  public void setRedirects(List<Page> redirect) {
    this.redirects = redirect;
    if (redirects != null) {
      isRedirect(true);
    }
  }

  /**
   * @param redirect Redirection.
   */
  public void addRedirect(Page redirect) {
    if (redirects == null) {
      redirects = new ArrayList<Page>(); 
    }
    redirects.add(redirect);
    isRedirect(true);
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
          ok = false;
          MagicWord magicRedirect = wikipedia.getWikiConfiguration().getMagicWordByName(MagicWord.REDIRECT);
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
    if ((to.getNamespace() != null) &&
        (to.getNamespace() == Namespace.CATEGORY) &&
        (!toTitle.startsWith(":"))) {
      toTitle = ":" + toTitle;
    }
    return toTitle;
  }

  // ==========================================================================
  // Analysis
  // ==========================================================================

  /**
   * Page analysis.
   */
  private PageAnalysis analysis;

  /**
   * Page analysis.
   * 
   * @param currentContents Current page content to take into account.
   * @param update True to update the analysis stored with the page.
   * @return Page analysis for the current page contents.
   */
  public PageAnalysis getAnalysis(String currentContents, boolean update) {
    if (currentContents == null) {
      return new PageAnalysis(this, null);
    }
    PageAnalysis result = null;
    if ((analysis == null) || (!currentContents.equals(analysis.getContents()))) {
      result = new PageAnalysis(this, currentContents);
      if ((update) || (analysis == null)) {
        analysis = result;
      }
    } else {
      result = analysis;
    }
    return result;
  }

  /**
   * @return Last page analysis.
   */
  public PageAnalysis getLastAnalysis() {
    return analysis;
  }

  // ==========================================================================
  // General methods
  // ==========================================================================

  /**
   * Returns a String representation of the page.
   * 
   * @return String representation of the page.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return title;
  }

  /**
   * Compares this page with the specified page for order.
   * 
   * @param bl The page to be compared.
   * @return a negative integer, zero, or a positive integer as this page
   *         is less than, equal to, or greater the specified page.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(Page bl) {
    int compare;

    // Name space
    if (namespace == null) {
      if (bl.namespace != null) {
        return -1;
      }
    } else if (bl.namespace == null) {
      return 1;
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
    } else if (bl.title == null) {
      return 1;
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
    } else if (bl.pageId == null) {
      return 1;
    } else {
      compare = pageId.compareTo(bl.pageId);
      if (compare != 0) {
        return compare;
      }
    }
    
    return 0;
  }

  /**
   * Indicates whether some other page is "equal to" this one.
   * 
   * @param o The reference page with which to compare.
   * @return <code>true</code> if this page is the same as the page argument;
   *         <code>false</code> otherwise.
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

  /**
   * Returns a hash code value for the page.
   * 
   * @return A hash code value for this page.
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
