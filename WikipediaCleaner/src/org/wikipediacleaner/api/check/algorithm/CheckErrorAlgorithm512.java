/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 511 of check wikipedia project.
 * Error 511: Interwiki link written as external link
 */
public class CheckErrorAlgorithm512 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Convert them to internal links"),
  };

  public CheckErrorAlgorithm512() {
    super("Interwiki link written as external link");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    List<Interwiki> interwikis = wiki.getWikiConfiguration().getInterwikis();
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      String article = null;
      String prefix = null;
      for (Interwiki interwiki : interwikis) {
        String tmp = interwiki.isArticleUrl(link.getLink());
        if (tmp != null) {
          if ((article == null) || (interwiki.getLanguage() != null)) {
            article = tmp;
            prefix = interwiki.getPrefix();
          }
        }
      }
      if ((article != null) && (article.length() > 0) &&
          (prefix != null) && (prefix.length() > 0)) {
        if (errors == null) {
          return true;
        }
        result = true;
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '[') &&
            (endIndex < contents.length()) && (contents.charAt(endIndex) == ']')) {
          beginIndex--;
          endIndex++;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis.getPage(), beginIndex, endIndex);
        errorResult.addReplacement(
            "[[:" + prefix + ":" + article + "|" + (link.getText() != null ? link.getText() : article) + "]]");
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public String botFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
