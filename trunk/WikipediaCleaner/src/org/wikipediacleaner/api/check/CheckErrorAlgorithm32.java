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

package org.wikipediacleaner.api.check;

import java.util.ArrayList;

import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 32 of check wikipedia project.
 * Error 32: Double pipe in one link.
 */
public class CheckErrorAlgorithm32 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm32() {
    super("Double pipe in one link");
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
    int beginIndex = contents.indexOf("[[", startIndex);
    int endIndex = 0;
    int pipe1Index = 0;
    int pipe2Index = 0;
    Namespace fileNamespace = Namespace.getNamespace(Namespace.IMAGE, page.getWikipedia().getNamespaces());
    while (startIndex < contents.length()) {
      // Update of begin index
      if ((beginIndex >= 0) && (beginIndex < startIndex)) {
        beginIndex = contents.indexOf("[[", startIndex);
      }
      if (beginIndex < 0) {
        startIndex = contents.length();
      } else {
        beginIndex += 2;
        
        // Update of end index
        if ((endIndex >= 0) && (endIndex < beginIndex)) {
          endIndex = contents.indexOf("]]", beginIndex);
        }
        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          
          // Update of pipe index
          if ((pipe1Index >= 0) && (pipe1Index < beginIndex)) {
            if (pipe2Index < 0) {
              pipe1Index = 0;
            } else if (pipe2Index < beginIndex) {
              pipe1Index = contents.indexOf("|", beginIndex);
              if (pipe1Index >= 0) {
                pipe2Index = contents.indexOf("|", pipe1Index + 1);
              } else {
                pipe2Index = -1;
              }
            } else {
              pipe1Index = pipe2Index;
              pipe2Index = contents.indexOf("|", pipe1Index + 1);
            }
          }

          if ((pipe1Index < 0) || (pipe2Index < 0)) {
            startIndex = contents.length();
          } else {
            int beginIndex2 = contents.indexOf("[[", beginIndex);
            if (pipe2Index < endIndex) {
              if ((beginIndex2 < 0) || (beginIndex2 > endIndex)) {
                boolean isImageNamespace= false;
                int namespaceIndex = contents.indexOf(":", beginIndex);
                if ((namespaceIndex > beginIndex) && (namespaceIndex < pipe1Index)) {
                  if ((fileNamespace != null) &&
                      (fileNamespace.isPossibleName(contents.substring(beginIndex, namespaceIndex).trim()))) {
                    isImageNamespace = true;
                  }
                }
                if (!isImageNamespace) {
                  if (errors == null) {
                    return true;
                  }
                  result = true;
                  errors.add(new CheckErrorResult(getShortDescription(), beginIndex, endIndex));
                }
                if (beginIndex2 < 0) {
                  startIndex = contents.length();
                } else {
                  startIndex = beginIndex2;
                }
              } else {
                startIndex = beginIndex2;
              }
            } else {
              if (beginIndex2 < 0) {
                startIndex = contents.length();
              } else {
                startIndex = beginIndex2;
              }
            }
          }
        }
      }
    }
    return result;
  }
}
