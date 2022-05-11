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
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysisUtils;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Processor for disambiguation warnings on talk pages.
 */
public class UnknownParameterWarningProcessor extends WarningProcessor {

  /**
   * Create a processor for disambiguation warnings on talk pages.
   * 
   * @param wiki Wiki.
   */
  public UnknownParameterWarningProcessor(final EnumWikipedia wiki) {
    super(wiki);
  }

  /**
   * Extract information about unknown parameters.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of unknown parameters).
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return List of unknown parameter errors.
   */
  @Override
  protected Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }

    // Prepare list of algorithms
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 564)); // Unknown parameter

    // Retrieve list of errors
    List<CheckErrorResult> errorResults = new ArrayList<>();
    for (CheckErrorAlgorithm algorithm : algorithms) {
      int errorNumber = algorithm.getErrorNumber();
      if (CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
        CheckErrorPage errorPage = AlgorithmError.analyzeError(algorithm, analysis);
        List<CheckErrorResult> results = errorPage.getResults();
        if (results != null) {
          errorResults.addAll(results);
        }
      }
    }
    Collections.sort(errorResults);

    // Compute list of elements for the warning
    List<String> elements = new ArrayList<>();
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
        String argumentValue = "";
        String chapterName = "";
        boolean keep = false;
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if (template != null) {
          templateName = template.getTemplateName();
          Parameter param = template.getParameterAtIndex(beginIndex);
          if (param != null) {
            argumentValue = analysis.getContents().substring(param.getNameStartIndex(), param.getEndIndex());
            PageElementTitle title = PageAnalysisUtils.getCurrentChapter(analysis, beginIndex);
            if (title != null) {
              chapterName = title.getTitle();
            }
            keep = true;
          }
        }
        if (keep) {
          elements.add(templateName);
          elements.add(argumentValue
              .replaceAll("\\:", "&#58;")
              .replaceAll("\\<", "&#60;")
              .replaceAll("\\=", "&#61;")
              .replaceAll("\\>", "&#62;")
              .replaceAll("\\[", "&#91;")
              .replaceAll("\\]", "&#93;")
              .replaceAll("\\{", "&#123;")
              .replaceAll("\\|", "&#124;")
              .replaceAll("\\}", "&#125;")
              .replaceAll("\\~", "&#126;")
              .replaceAll("\n", "\u21b5")
              .trim());
          elements.add(chapterName);
        }
      }
    }
    return elements;
  }
}
