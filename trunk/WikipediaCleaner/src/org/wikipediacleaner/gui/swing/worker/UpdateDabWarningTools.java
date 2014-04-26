/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.InternalLinkCount;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating disambiguation warnings.
 */
public class UpdateDabWarningTools extends UpdateWarningTools {

  private final static Log log = LogFactory.getLog(UpdateDabWarningTools.class);

  private final Map<String, Page> dabPages;
  private final Map<String, Page> nonDabPages;

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateDabWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateDabWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateDabWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, createWarning, automaticEdit);
    this.dabPages = new HashMap<String, Page>();
    this.nonDabPages = new HashMap<String, Page>();
  }

  /**
   * Load all disambiguation pages.
   */
  public void preloadDabPages() {
    dabPages.clear();
    nonDabPages.clear();
    try {
      wiki.loadDisambiguationPages(api);
    } catch (APIException e) {
      log.error("Error preloading disambiguation pages", e);
    }
  }

  /**
   * Update disambiguation warning for a list of pages.
   * 
   * @param pages List of pages.
   * @param contentsAvailable True if contents is already available in pages.
   * @param linksAvailable True if links are already available in pages.
   * @param dabInformationAvailable True if disambiguation information is already available in pages.
   * @param creators For each page title, user who has created the page.
   * @param modifiers For each page title, users who have modified the page.
   * @param stats Statistics.
   * @throws APIException
   */
  public void updateDabWarning(
      List<Page> pages, boolean contentsAvailable,
      boolean linksAvailable, boolean dabInformationAvailable,
      Map<String, String> creators,
      Map<String, List<String>> modifiers,
      Stats stats) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);

    // Retrieving links in each page
    if (!linksAvailable) {
      for (Page page : pages) {
        mw.retrieveAllLinks(wiki, page, Namespace.MAIN, null, false, false);
      }
      mw.block(true);
      if (shouldStop()) {
        return;
      }
    }

    // Retrieving disambiguation information in each page
    boolean hasDisambiguationLink = false;
    if (!dabInformationAvailable) {
      if (!wiki.isDisambiguationPagesLoaded()) {
        List<Page> tmpPages = new ArrayList<Page>();
        for (Page page : pages) {
          for (int numLink = 0; numLink < page.getLinks().size(); numLink++) {
            Page link = page.getLinks().get(numLink);
            if (dabPages.containsKey(link.getTitle())) {
              page.getLinks().set(numLink, dabPages.get(link.getTitle()));
              hasDisambiguationLink = true;
            } else if (nonDabPages.containsKey(link.getTitle())) {
              page.getLinks().set(numLink, nonDabPages.get(link.getTitle()));
            } else {
              tmpPages.add(link);
            }
          }
        }
        if (!tmpPages.isEmpty()) {
          mw.retrieveDisambiguationInformation(wiki, tmpPages, null, false, false, true);
        }
        for (Page page : tmpPages) {
          if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
            dabPages.put(page.getTitle(), page);
            hasDisambiguationLink = true;
          } else {
            nonDabPages.put(page.getTitle(), page);
          }
        }
      } else {
        for (Page page : pages) {
          List<Page> links = page.getLinksWithRedirect();
          for (int numLink = 0; numLink < links.size(); numLink++) {
            Page link = links.get(numLink);
            if (Boolean.TRUE.equals(wiki.isDisambiguationPage(link))) {
              link.setDisambiguationPage(Boolean.TRUE);
              hasDisambiguationLink = true;
            } else {
              link.setDisambiguationPage(Boolean.FALSE);
            }
          }
        }
      }
      if (shouldStop()) {
        return;
      }
    }

    // Retrieving page contents
    if (hasDisambiguationLink && !contentsAvailable) {
      List<Page> tmpPages = new ArrayList<Page>();
      for (Page page : pages) {
        boolean toAdd = false;
        for (Page link : page.getLinks()) {
          if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
            toAdd = true;
          }
        }
        if (toAdd) {
          tmpPages.add(page);
        }
      }
      if (!tmpPages.isEmpty()) {
        mw.retrieveContents(wiki, tmpPages, true, false, false, false);
      }
    }

    // Load talk pages and "To do" sub pages
    Map<Page, Page> mapTalkPages = new HashMap<Page, Page>();
    Map<Page, Page> mapTodoSubpages = new HashMap<Page, Page>();
    for (Page page : pages) {
      Page talkPage = page.getTalkPage();
      mapTalkPages.put(page, talkPage);
      String todoSubpageAttr = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
      if (todoSubpageAttr != null) {
        Page todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        mapTodoSubpages.put(page, todoSubpage);
      }
    }
    if (section0) {
      mw.retrieveSectionContents(wiki, mapTalkPages.values(), 0, false);
    } else {
      mw.retrieveContents(wiki, mapTalkPages.values(), false, false, false, false);
    }
    mw.retrieveContents(wiki, mapTodoSubpages.values(), true, false, false, false);
    if (mw.shouldStop()) {
      return;
    }

    // Update disambiguation warning
    for (Page page : pages) {
      PageAnalysis pageAnalysis = page.getAnalysis(page.getContents(), true);
      boolean updated = updateDabWarning(
          pageAnalysis, page.getRevisionId(),
          mapTalkPages.get(page),
          mapTodoSubpages.get(page),
          (creators != null) ? creators.get(page.getTitle()) : null,
          (modifiers != null) ? modifiers.get(page.getTitle()) : null,
          stats);
      if (stats != null) {
        stats.addAnalyzedPage(page);
        if (updated) {
          stats.addUpdatedPage(page);
        }
      }
    }
    return;
  }

  /**
   * Update disambiguation warning for a page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param talkPage (Optional) Talk page with contents of section 0.
   * @param todoSubpage (Optional) Todo sub page with contents.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  public boolean updateDabWarning(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page talkPage, Page todoSubpage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    if ((pageAnalysis == null) || (pageAnalysis.getPage() == null)) {
      return false;
    }
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if ((todoTemplates == null) ||
        (todoTemplates.isEmpty())) {
      return false;
    }
    Page page = pageAnalysis.getPage();

    // Retrieving talk page contents
    if (talkPage == null) {
      talkPage = page.getTalkPage();
      setText(GT._("Retrieving page contents - {0}", talkPage.getTitle()));
      if (section0) {
        api.retrieveSectionContents(wiki, talkPage, 0);
      } else {
        api.retrieveContents(wiki, Collections.singletonList(talkPage), false, false);
      }
    }

    // "To do" sub-page
    String todoSubpageAttr = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
    if (todoSubpageAttr != null) {

      // Retrieving "To do" sub-page contents
      if (todoSubpage == null) {
        todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        setText(GT._("Retrieving page contents - {0}", todoSubpage.getTitle()));
        api.retrieveContents(wiki, Collections.singletonList(todoSubpage), false, false);
      }

      // If we force the use of "To do" sub-page, the disambiguation warning must be on it
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.MAIN)) {
        if (configuration.getBoolean(WPCConfigurationBoolean.TODO_SUBPAGE_FORCE)) {
          return manageDabWarningOnTodoSubpage(
              pageAnalysis, pageRevId, todoSubpage, talkPage,
              creator, modifiers, stats);
        }
      } else if (configuration.getBoolean(WPCConfigurationBoolean.TODO_SUBPAGE_FORCE_OTHER)) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If "To do" sub-page exists, the disambiguation warning must be on it
      if (Boolean.TRUE.equals(todoSubpage.isExisting())) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If talk page has a template linking to the "To do" sub-page,
      // the disambiguation warning must be on the "To do" sub-page
      PageElementTemplate templateTodoLink = getExistingTemplateTodoLink(talkPage, talkPage.getContents());
      if (templateTodoLink != null) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If talk page has a link to the "To do" sub-page,
      // the disambiguation warning must be on the "To do" sub-page
      /*api.retrieveLinks(wikipedia, talkPage, talkPage.getNamespace());
      if (talkPage.getLinks() != null) {
        for (Page link : talkPage.getLinks()) {
          if (Page.areSameTitle(link.getTitle(), todoSubpage.getTitle())) {
            return manageDabWarningOnTodoSubpage(pageAnalysis, pageRevId, todoSubpage, talkPage);
          }
        }
      }*/
    }

    return manageDabWarningOnTalkPage(
        pageAnalysis, pageRevId, talkPage,
        creator, modifiers, stats);
  }

  /**
   * Update disambiguation warning on the "To do" sub-page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param todoSubpage "To do" sub-page.
   * @param talkPage Talk page.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTodoSubpage(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page todoSubpage, Page talkPage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    Collection<String> dabLinks = findDabLinks(pageAnalysis, talkPage, todoSubpage);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result |= removeWarningOnTodoSubpage(todoSubpage);
      result |= removeWarningOnTalkPage(talkPage);
      if (stats != null) {
        stats.addRemovedWarning(pageAnalysis.getPage());
      }
    } else {
      result |= updateWarningOnTodoSubpage(
          pageRevId, todoSubpage, dabLinks, creator, modifiers);
      if (createWarning) {
        result |= cleanWarningOnTalkPage(talkPage, dabLinks);
      }
      if (stats != null) {
        stats.addLinks(pageAnalysis.getPage(), dabLinks.size());
      }
    }
    return result;
  }

  /**
   * Update disambiguation warning on the talk page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param talkPage Talk page.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTalkPage(
      PageAnalysis pageAnalysis, Integer pageRevId, Page talkPage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    Collection<String> dabLinks = findDabLinks(pageAnalysis, talkPage, null);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result = removeWarningOnTalkPage(talkPage);
      if (stats != null) {
        stats.addRemovedWarning(pageAnalysis.getPage());
      }
    } else {
      result |= updateWarningOnTalkPage(
          pageAnalysis, pageRevId, talkPage, dabLinks, creator, modifiers);
      if (stats != null) {
        stats.addLinks(pageAnalysis.getPage(), dabLinks.size());
      }
    }
    return result;
  }

  /**
   * Extract links to disambiguation pages.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @return List of links to disambiguation pages.
   */
  private Collection<String> findDabLinks(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }
    List<String> dabLinks = new ArrayList<String>();
    List<Page> links = analysis.getPage().getLinks();
    if (links != null) {

      // List disambiguation links in the page
      analysis.countLinks(links);
      for (Page link : links) {
        if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
          InternalLinkCount linkCount = analysis.getLinkCount(link);
          if (linkCount != null) {
            if ((linkCount.getInternalLinkCount() > 0) ||
                (linkCount.getIncorrectTemplateCount() > 0) ||
                (linkCount.getHelpNeededCount() > 0)) {
              dabLinks.add(link.getTitle());
            }
          }
        }
      }

      // Remove links marked as normal
      cleanDabList(dabLinks, talkPage);
      cleanDabList(dabLinks, todoSubpage);
    }
    Collections.sort(dabLinks);
    return dabLinks;
  }

  /**
   * Remove disambiguation links marked as normal from the list.
   * 
   * @param dabList List of disambiguation links.
   * @param page Page containing the list of normal disambiguation links.
   */
  private void cleanDabList(Collection<String> dabList, Page page) {
    if ((dabList == null) || (page == null)) {
      return;
    }
    String okTemplate = configuration.getString(WPCConfigurationString.DAB_OK_TEMPLATE);
    if ((okTemplate == null) || okTemplate.trim().isEmpty()) {
      return;
    }
    okTemplate = okTemplate.trim();
    PageAnalysis analysis = page.getAnalysis(page.getContents(), false);
    List<PageElementTemplate> templates = analysis.getTemplates(okTemplate);
    if ((templates == null) || templates.isEmpty()) {
      return;
    }
    for (PageElementTemplate template : templates) {
      boolean done = false;
      int numParam = 1;
      while (!done) {
        String link = template.getParameterValue(Integer.toString(numParam));
        if (link == null) {
          done = true;
        } else {
          Iterator<String> itDab = dabList.iterator();
          while (itDab.hasNext()) {
            String dab = itDab.next();
            if (Page.areSameTitle(dab, link)) {
              itDab.remove();
            }
          }
          numParam++;
        }
      }
    }
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  @Override
  protected WPCConfigurationString getWarningTemplate() {
    return WPCConfigurationString.DAB_WARNING_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the warning template comment.
   */
  @Override
  protected WPCConfigurationString getWarningTemplateComment() {
    return WPCConfigurationString.DAB_WARNING_TEMPLATE_COMMENT;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTitleNewArticle() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_WITH_DAB_TITLE;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTitleNewArticleModified() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIED_WITH_DAB_TITLE;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTitleNewArticleModifier() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIER_WITH_DAB_TITLE;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTemplateNewArticle() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_WITH_DAB_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTemplateNewArticleModified() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIED_WITH_DAB_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTemplateNewArticleModifier() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIER_WITH_DAB_TEMPLATE;
  }

  /**
   * @return True if section 0 of the talk page should be used.
   */
  @Override
  protected boolean useSection0() {
    return configuration.getBoolean(WPCConfigurationBoolean.DAB_WARNING_SECTION_0);
  }

  /**
   * @return Comment when warning is removed.
   */
  @Override
  protected String getWarningCommentDone() {
    return configuration.getDisambiguationWarningCommentDone();
  }

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  @Override
  protected String getWarningComment(Collection<String> elements) {
    return configuration.getDisambiguationWarningComment(elements);
  }

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  @Override
  protected String getMessageRemoveWarning(String title) {
    return GT._("Removing disambiguation warning - {0}", title);
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._("Updating disambiguation warning - {0}", title);
  }
}
