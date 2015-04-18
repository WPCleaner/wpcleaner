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
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageAnalysisUtils;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating ISBN warnings.
 */
public class UpdateDuplicateArgsWarningTools extends UpdateWarningTools {

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateDuplicateArgsWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateDuplicateArgsWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateDuplicateArgsWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, createWarning, automaticEdit);
  }

  /**
   * Retrieve information in the pages to construct the warning.
   * 
   * @param pages List of pages.
   * @return True if information was retrieved.
   * @throws APIException
   */
  @Override
  protected boolean retrievePageInformation(
      List<Page> pages) throws APIException {

    // Retrieving page contents
    if (!getContentsAvailable()) {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);
      mw.retrieveContents(wiki, pages, true, false, false, true);
    }

    return true;
  }

  /**
   * Extract information about ISBN with errors.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of ISBN errors).
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return List of ISBN errors.
   */
  @Override
  protected Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }

    // Prepare list of algorithms
    List<CheckErrorAlgorithm> algorithms = new ArrayList<CheckErrorAlgorithm>();
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 524)); // Duplicate template args

    // Retrieve list of errors
    List<CheckErrorResult> errorResults = new ArrayList<CheckErrorResult>();
    for (CheckErrorAlgorithm algorithm : algorithms) {
      int errorNumber = algorithm.getErrorNumber();
      if (CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
        CheckErrorPage errorPage = CheckError.analyzeError(algorithm, analysis);
        List<CheckErrorResult> results = errorPage.getResults();
        if (results != null) {
          errorResults.addAll(results);
        }
      }
    }
    Collections.sort(errorResults);

    // Compute list of elements for the warning
    List<String> elements = new ArrayList<String>();
    String contents = analysis.getContents();
    for (CheckErrorResult errorResult : errorResults) {
      if (ErrorLevel.ERROR.equals(errorResult.getErrorLevel())) {
        int beginIndex = errorResult.getStartPosition();
        while ((beginIndex < contents.length()) &&
               (contents.charAt(beginIndex) != '|') &&
               (contents.charAt(beginIndex) != '}')) {
          beginIndex++;
        }
        if ((beginIndex < contents.length()) &&
            (contents.charAt(beginIndex) == '|')) {
          beginIndex++;
        }
        String templateName = null;
        String argumentName = null;
        String chapterName = "";
        boolean keep = false;
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if (template != null) {
          templateName = template.getTemplateName();
          Parameter param = template.getParameterAtIndex(beginIndex);
          if (param != null) {
            argumentName = param.getComputedName();
            PageElementTitle title = PageAnalysisUtils.getCurrentChapter(analysis, beginIndex);
            if (title != null) {
              chapterName = title.getTitle();
            }
            keep = true;
          }
        }
        if (keep) {
          elements.add(templateName);
          elements.add(argumentName);
          elements.add(chapterName);
        }
      }
    }
    return elements;
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  @Override
  protected WPCConfigurationString getWarningTemplate() {
    return WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the warning template comment.
   */
  @Override
  protected WPCConfigurationString getWarningTemplateComment() {
    return WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE_COMMENT;
  }

  /**
   * @return True if section 0 of the talk page should be used.
   */
  @Override
  protected boolean useSection0() {
    return configuration.getBoolean(WPCConfigurationBoolean.DUPLICATE_ARGS_WARNING_SECTION_0);
  }

  /**
   * @return Comment when warning is removed.
   */
  @Override
  protected String getWarningCommentDone() {
    return configuration.getDuplicateArgsWarningCommentDone();
  }

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  @Override
  protected String getWarningComment(Collection<String> elements) {
    Collection<String> arguments = new ArrayList<String>();
    int i = 0;
    for (String element : elements) {
      if (i % 3 == 1) {
        arguments.add(element);
      }
      i++;
    }
    return configuration.getDuplicateArgsWarningComment(arguments);
  }

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  @Override
  protected String getMessageRemoveWarning(String title) {
    return GT._("Removing duplicate arguments warning - {0}", title);
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._("Updating duplicate arguments warning - {0}", title);
  }
}
