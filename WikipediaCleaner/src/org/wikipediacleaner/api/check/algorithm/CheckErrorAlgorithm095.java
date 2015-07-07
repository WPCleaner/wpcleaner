/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;


/**
 * Algorithm for analyzing error 95 of check wikipedia project.
 * Error 95: Editor's signature or link to user space
 */
public class CheckErrorAlgorithm095 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm095() {
    super("Editor's signature or link to user space");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isInMainNamespace()) {
      return false;
    }

    // Analyze each internal link
    boolean result = false;
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return result;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    for (PageElementInternalLink link : links) {
      String linkDest = link.getLink();
      if ((linkDest != null) &&
          (linkDest.trim().length() > 0) &&
          (linkDest.indexOf(':') >= 0)) {
        Page page = DataManager.getPage(wiki, linkDest, null, null, null);
        Integer namespace = page.getNamespace();
        if ((namespace != null) &&
            ((namespace.intValue() == Namespace.USER) ||
             (namespace.intValue() == Namespace.USER_TALK))) {
          if (errors == null) {
            return true;
          }
          result = true;
          int beginIndex = link.getBeginIndex();
          int endIndex = link.getEndIndex();
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
