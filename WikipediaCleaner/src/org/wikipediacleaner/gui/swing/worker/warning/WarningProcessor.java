/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Abstract processor for warnings on talk pages.
 */
public abstract class WarningProcessor {

  /** Wiki. */
  protected final EnumWikipedia wiki;

  /** Wiki configuration. */
  protected final WPCConfiguration configuration;

  /** Map for errors. */
  private Map<String, List<String>> errorsMap;

  /**
   * Create a processor for warnings on talk pages.
   * 
   * @param wiki Wiki.
   */
  public WarningProcessor(EnumWikipedia wiki) {
    this.wiki = wiki;
    this.configuration = wiki.getConfiguration();
  }

  /**
   * Construct elements for the warning.
   * 
   * @param analysis Page analysis.
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return Warning elements.
   */
  protected abstract Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage);

  // ==========================================================================
  // Store errors
  // ==========================================================================

  /**
   * Initialize the errors map.
   */
  public void prepareErrorsMap() {
    this.errorsMap = new HashMap<>();
  }

  /**
   * @return Errors map.
   */
  public Map<String, List<String>> getErrorsMap() {
    return errorsMap;
  }

  /**
   * Memorize an error.
   * 
   * @param error Error to memorize. 
   * @param title Page title in which the error is present.
   */
  protected void memorizeError(String error, String title) {
    if ((errorsMap == null) || (error == null) || (title == null)) {
      return;
    }
    List<String> titles = errorsMap.get(error);
    if (titles == null) {
      titles = new ArrayList<>();
      errorsMap.put(error, titles);
    }
    titles.add(title);
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  protected abstract WPCConfigurationString getWarningTemplate();

  /**
   * @return Configuration parameter for the warning template comment.
   */
  protected abstract WPCConfigurationString getWarningTemplateComment();

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  protected WPCConfigurationString getMessageTitleNewArticle() {
    return null;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  protected WPCConfigurationString getMessageTitleNewArticleModified() {
    return null;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  protected WPCConfigurationString getMessageTitleNewArticleModifier() {
    return null;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  protected WPCConfigurationString getMessageTemplateNewArticle() {
    return null;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  protected WPCConfigurationString getMessageTemplateNewArticleModified() {
    return null;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  protected WPCConfigurationString getMessageTemplateNewArticleModifier() {
    return null;
  }

  /**
   * @return Configuration parameter telling if section 0 of the talk page should be used.
   */
  protected abstract WPCConfigurationBoolean getUseSection0();

  /**
   * @return Comment when warning is removed.
   */
  protected abstract String getWarningCommentDone();

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  protected abstract String getWarningComment(Collection<String> elements);

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  protected abstract String getMessageRemoveWarning(String title);

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  protected abstract String getMessageUpdateWarning(String title);
}
