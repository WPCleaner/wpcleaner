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

import java.util.List;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TagData;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 79 of check wikipedia project.
 * Error 79: External link without description.
 */
public class CheckErrorAlgorithm079 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm079() {
    super("External link without description");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    boolean result = false;
    result |= analyzeProtocol("[http://", page, contents, errors);
    result |= analyzeProtocol("[ftp://", page, contents, errors);
    result |= analyzeProtocol("[https://", page, contents, errors);
    return result;
  }

  /**
   * Check for errors for on protocol.
   * 
   * @param protocol Protocol.
   * @param page Page.
   * @param contents Page contents.
   * @return
   */
  private boolean analyzeProtocol(
      String protocol, Page page, String contents,
      List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf(protocol, startIndex);
      if (startIndex >= 0) {
        int endIndex = contents.indexOf("]", startIndex);
        int spaceIndex = contents.indexOf(" ", startIndex);
        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          if ((spaceIndex < 0) || (spaceIndex > endIndex)) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                page, startIndex, endIndex + 1); 
            boolean isInRef = false;
            TagData previousStartRef = findPreviousStartTag(page, contents, "ref", startIndex);
            if (previousStartRef != null) {
              TagData previousEndRef = findPreviousEndTag(page, contents, "ref", startIndex);
              if ((previousEndRef == null) || (previousEndRef.getEndIndex() < previousStartRef.getEndIndex())) {
                TagData nextEndRef = findNextEndTag(page, contents, "ref", endIndex);
                if (nextEndRef != null) {
                  TagData nextStartRef = findNextStartTag(page, contents, "ref", startIndex);
                  if ((nextStartRef == null) || (nextEndRef.getStartIndex() < nextStartRef.getStartIndex())) {
                    isInRef = true;
                  }
                }
              }
            }
            String url = contents.substring(startIndex + 1, endIndex).trim();
            errorResult.addPossibleAction(
                GT._("Add a description..."),
                new AddTextActionProvider(
                    "[" + url + " ", "]", url,
                    GT._("What description would like to use for the external link ?"),
                    "[]"));
            if (!isInRef) {
              errorResult.addReplacement(
                  "<ref>" + url + "</ref>",
                  GT._("Convert into <ref> tag"));
              errorResult.addPossibleAction(
                  GT._("Add a description and convert into <ref> tag"),
                  new AddTextActionProvider(
                      "<ref>[" + url + " ", "]</ref>", url,
                      GT._("What description would like to use for the external link ?"),
                      "[]"));
            } else {
              errorResult.addReplacement(url);
            }
            errorResult.addPossibleAction(new SimpleAction(GT._("External viewer"), new PageViewAction(url)));
            errors.add(errorResult);
            startIndex = endIndex + 1;
          } else {
            startIndex = endIndex + 1;
          }
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
