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
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.Page;
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
    HashMap<String, RefElement> refs = new HashMap<String, RefElement>();
    ArrayList<String> names = new ArrayList<String>();
    ArrayList<Actionnable> existingNames = new ArrayList<Actionnable>();
    while (startIndex < contents.length()) {
      if (contents.charAt(startIndex) == '<') {
        int beginIndex = startIndex;
        int currentIndex = beginIndex + 1;
        if (contents.startsWith("ref", currentIndex)) {
          currentIndex += 3;
          String name = null;
          while ((currentIndex < contents.length()) &&
                 (contents.charAt(currentIndex) != '/') &&
                 (contents.charAt(currentIndex) != '>') &&
                 (contents.charAt(currentIndex) != '<')) {
            if (contents.startsWith(" name=", currentIndex)) {
              currentIndex += 6;
              char separator = contents.charAt(currentIndex);
              if ((separator == '"') || (separator == '\'')) {
                currentIndex++;
                int beginName = currentIndex;
                while ((currentIndex < contents.length()) &&
                       (contents.charAt(currentIndex) != separator) &&
                       (contents.charAt(currentIndex) != '/') &&
                       (contents.charAt(currentIndex) != '>') &&
                       (contents.charAt(currentIndex) != '<')) {
                  currentIndex++;
                }
                if (contents.charAt(currentIndex) == separator) {
                  name = contents.substring(beginName, currentIndex).trim();
                  if (!names.contains(name)) {
                    names.add(name);
                  }
                }
              }
            } else {
              currentIndex++;
            }
          }
          boolean simpleTag = false;
          if ((currentIndex < contents.length()) &&
              (contents.charAt(currentIndex) == '/')) {
            simpleTag = true;
            currentIndex++;
          }
          if ((currentIndex < contents.length()) &&
              (contents.charAt(currentIndex) == '>')) {
            currentIndex++;
            int beginRefIndex = currentIndex;
            int endRefIndex = simpleTag ? beginRefIndex : contents.indexOf("</ref>", beginRefIndex);
            if (endRefIndex < 0) {
              currentIndex = contents.length();
            } else {
              currentIndex = endRefIndex + (simpleTag ? 0 : 6);
              String reference = contents.substring(beginRefIndex, endRefIndex).trim();
              if (reference.length() > 0) {
                RefElement refElement = refs.get(reference);
                if (refElement == null) {
                  refElement = new RefElement(reference, beginIndex, currentIndex, name);
                  refs.put(reference, refElement);
                } else {
                  if (errors == null) {
                    return true;
                  }
                  result = true;
                  if (refElement.errorResult == null) {
                    refElement.errorResult = new CheckErrorResult(
                        getShortDescription(),
                        refElement.begin, refElement.end,
                        (refElement.name == null) ?
                            CheckErrorResult.ErrorLevel.WARNING :
                            CheckErrorResult.ErrorLevel.CORRECT);
                    refElement.errorResult.addPossibleAction(
                        new CompositeAction(GT._("Existing references"), existingNames));
                    errors.add(refElement.errorResult);
                  }
                  CheckErrorResult errorResult = new CheckErrorResult(
                      getShortDescription(), beginIndex, currentIndex);
                  if (refElement.name != null) {
                    errorResult.addReplacement("<ref name=\"" + refElement.name + "\"/>");
                  }
                  errorResult.addPossibleAction(
                      new CompositeAction(GT._("Existing references"), existingNames));
                  errors.add(errorResult);
                }
              }
            }
          }
        }
        startIndex = currentIndex;
      } else {
        startIndex++;
      }
    }
    Collections.sort(names);
    for (String name : names) {
      existingNames.add(new SimpleAction(name, new NoOpAction()));
    }
    return result;
  }

  /**
   * Class to hold informations about a ref element.
   */
  static class RefElement {
    String reference;
    int begin;
    int end;
    String name;
    CheckErrorResult errorResult;

    /**
     * @param reference Reference text.
     * @param begin Begin position.
     * @param end End position.
     * @param name Reference name.
     */
    public RefElement(String reference, int begin, int end, String name) {
      this.reference = reference;
      this.begin = begin;
      this.end = end;
      this.name = name;
    }
  }
}
