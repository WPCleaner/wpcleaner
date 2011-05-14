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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.gui.swing.action.NoOpAction;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 81 of check wikipedia project.
 * Error 81: Reference duplication.
 */
public class CheckErrorAlgorithm081 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm081() {
    super("Reference duplication");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
    HashMap<String, PageElementTag> refs = new HashMap<String, PageElementTag>();
    HashMap<PageElementTag, CheckErrorResult> errorResults = new HashMap<PageElementTag, CheckErrorResult>();
    ArrayList<String> names = new ArrayList<String>();
    ArrayList<Actionnable> existingNames = new ArrayList<Actionnable>();
    String contents = pageAnalysis.getContents();
    while ((startIndex < contents.length()) && (startIndex >= 0)) {
      if (contents.charAt(startIndex) == '<') {
        PageElementTag ref = PageElementTag.analyzeBlock("ref", contents, startIndex);
        if (ref != null) {
          startIndex = ref.getEndTagEndIndex();
          String reference = ref.getText();
          if ((reference != null) && (reference.length() > 0)) {
            PageElementTag previousRef = refs.get(reference);
            if (previousRef == null) {
              refs.put(reference, ref);
            } else {
              if (errors == null) {
                return true;
              }
              result = true;
              String previousName = previousRef.getParameter("name");
              String previousGroup = previousRef.getParameter("group");
              if (errorResults.get(previousRef) == null) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    pageAnalysis.getPage(),
                    previousRef.getStartTagBeginIndex(), previousRef.getEndTagEndIndex(),
                    (previousName == null) ?
                        CheckErrorResult.ErrorLevel.WARNING :
                        CheckErrorResult.ErrorLevel.CORRECT);
                String url = null;
                String text = previousRef.getText();
                if (text != null) {
                  int httpIndex = text.indexOf("http://");
                  if (httpIndex < 0) {
                    httpIndex = text.indexOf("https://");
                  }
                  if (httpIndex >= 0) {
                    int spaceIndex = text.indexOf(' ', httpIndex);
                    if (spaceIndex < 0) {
                      url = text.substring(httpIndex);
                    } else {
                      url = text.substring(httpIndex, spaceIndex);
                    }
                  }
                }
                if (previousName == null) {
                  errorResult.addPossibleAction(
                      GT._("Give a name to the <ref> tag"),
                      new AddTextActionProvider(
                          previousRef.getPartBeforeParameters() + " name=\"",
                          "\"" + previousRef.getPartFromParameters(),
                          url,
                          GT._("What name would like to use for the <ref> tag ?"),
                          "[]\""));
                }
                errorResult.addPossibleAction(
                    new CompositeAction(GT._("Existing references"), existingNames));
                errorResult.addPossibleAction(new SimpleAction(
                    GT._("External Viewer"), new PageViewAction(url)));
                errors.add(errorResult);
                errorResults.put(previousRef, errorResult);
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(),
                  ref.getStartTagBeginIndex(), ref.getEndTagEndIndex());
              if (previousName != null) {
                if (previousGroup != null) {
                  errorResult.addReplacement("<ref group=" + previousGroup + " name=" + previousName + "/>");
                } else {
                  errorResult.addReplacement("<ref name=" + previousName + "/>");
                }
              }
              errorResult.addPossibleAction(
                  new CompositeAction(GT._("Existing references"), existingNames));
              errors.add(errorResult);
            }
          }
        } else {
          startIndex = contents.indexOf('<', startIndex + 1);
        }
      } else {
        startIndex = contents.indexOf('<', startIndex + 1);
      }
    }
    Collections.sort(names);
    for (String name : names) {
      existingNames.add(new SimpleAction(name, new NoOpAction()));
    }
    return result;
  }
}
