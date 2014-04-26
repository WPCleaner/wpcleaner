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

  /** True if contents is already available in pages. */
  private boolean contentsAvailable;

  /** True if links are already available in pages. */
  private boolean linksAvailable;

  /** True if disambiguation information is already available in pages. */
  private boolean dabInformationAvailable;

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
   * @param available True if contents is already available in pages.
   */
  public void setContentsAvailable(boolean available) {
    this.contentsAvailable = available;
  }

  /**
   * @param available True if links are already available in pages.
   */
  public void setLinksAvailable(boolean available) {
    this.linksAvailable = available;
  }

  /**
   * @param available True if disambiguation information is already available in pages.
   */
  public void setDabInformationAvailable(boolean available) {
    this.dabInformationAvailable = available;
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
   * Retrieve information in the pages to construct the warning.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  @Override
  protected void retrievePageInformation(
      List<Page> pages) throws APIException {
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
  }

  /**
   * Extract links to disambiguation pages.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return List of links to disambiguation pages.
   */
  @Override
  protected Collection<String> constructWarningElements(
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
