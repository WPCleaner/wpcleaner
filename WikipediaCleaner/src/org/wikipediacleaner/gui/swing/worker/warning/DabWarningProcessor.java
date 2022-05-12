/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.InternalLinkCount;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;

/**
 * Processor for disambiguation warnings on talk pages.
 */
public class DabWarningProcessor extends WarningProcessor {

  /**
   * Create a processor for disambiguation warnings on talk pages.
   * 
   * @param wiki Wiki.
   */
  public DabWarningProcessor(final EnumWikipedia wiki) {
    super(wiki);
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
    List<String> dabLinks = new ArrayList<>();
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
              String error = link.getTitle();
              dabLinks.add(error);
              memorizeError(error, analysis.getPage().getTitle());
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
   * @return Configuration parameter telling if section 0 of the talk page should be used.
   */
  @Override
  protected WPCConfigurationBoolean getUseSection0() {
    return WPCConfigurationBoolean.DAB_WARNING_SECTION_0;
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
    return GT._T("Removing disambiguation warning - {0}", title);
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._T("Updating disambiguation warning - {0}", title);
  }
}
