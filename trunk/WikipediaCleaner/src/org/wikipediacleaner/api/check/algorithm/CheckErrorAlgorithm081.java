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
import java.util.Collections;
import java.util.HashMap;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TagBlock;
import org.wikipediacleaner.gui.swing.action.NoOpAction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 81 of check wikipedia project.
 * Error 81: Reference duplication.
 */
public class CheckErrorAlgorithm081 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm081() {
    super("Reference duplication");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
    HashMap<String, TagBlock> refs = new HashMap<String, TagBlock>();
    HashMap<TagBlock, CheckErrorResult> errorResults = new HashMap<TagBlock, CheckErrorResult>();
    ArrayList<String> names = new ArrayList<String>();
    ArrayList<Actionnable> existingNames = new ArrayList<Actionnable>();
    while ((startIndex < contents.length()) && (startIndex >= 0)) {
      if (contents.charAt(startIndex) == '<') {
        TagBlock ref = TagBlock.analyzeBlock("ref", contents, startIndex);
        if (ref != null) {
          startIndex = ref.getEndTagEndIndex() + 1;
          String reference = ref.getText();
          if ((reference != null) && (!reference.isEmpty())) {
            TagBlock previousRef = refs.get(reference);
            if (previousRef == null) {
              refs.put(reference, ref);
            } else {
              if (errors == null) {
                return true;
              }
              result = true;
              String previousName = previousRef.getParameter("name");
              if (errorResults.get(previousRef) == null) {
                CheckErrorResult errorResult = new CheckErrorResult(
                    getShortDescription(),
                    previousRef.getStartTagBeginIndex(), previousRef.getEndTagEndIndex() + 1,
                    (previousName == null) ?
                        CheckErrorResult.ErrorLevel.WARNING :
                        CheckErrorResult.ErrorLevel.CORRECT);
                if (previousName == null) {
                  errorResult.addPossibleAction(
                      GT._("Give a name to the <ref> tag"),
                      new AddTextActionProvider(
                          previousRef.getPartBeforeParameters() + " name=\"",
                          "\"" + previousRef.getPartFromParameters(),
                          null,
                          GT._("What name would like to use for the <ref> tag ?"),
                          "[]\""));
                }
                errorResult.addPossibleAction(
                    new CompositeAction(GT._("Existing references"), existingNames));
                errors.add(errorResult);
                errorResults.put(previousRef, errorResult);
              }
              CheckErrorResult errorResult = new CheckErrorResult(
                  getShortDescription(),
                  ref.getStartTagBeginIndex(), ref.getEndTagEndIndex() + 1);
              if (previousName != null) {
                errorResult.addReplacement("<ref name=" + previousName + "/>");
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
