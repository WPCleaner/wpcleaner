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
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTagFull;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerReferenceName;
import org.wikipediacleaner.utils.TextProviderUrlTitle;


/**
 * Algorithm for analyzing error 81 of check wikipedia project.
 * Error 81: Reference duplication.
 */
public class CheckErrorAlgorithm081 extends CheckErrorAlgorithmBase {

  /**
   * String checker for the reference name.
   */
  private final StringChecker nameChecker;

  public CheckErrorAlgorithm081() {
    super("Reference duplication");
    nameChecker = new StringCheckerReferenceName();
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
    HashMap<String, HashMap<String, ArrayList<PageElementTagFull>>> refs = new HashMap<String, HashMap<String, ArrayList<PageElementTagFull>>>();
    String contents = pageAnalysis.getContents();

    // Find all ref tags and organize them by group / value
    while (startIndex < contents.length()) {
      PageElementTagFull ref = PageContents.findNextTag(
          pageAnalysis.getPage(), contents, "ref", startIndex);
      if (ref == null) {
        startIndex = contents.length();
      } else {
        startIndex = ref.getEndTagEndIndex();

        // Analyze group
        String parameterGroup = ref.getParameter("group");
        if (parameterGroup == null) {
          parameterGroup = "";
        }
        HashMap<String, ArrayList<PageElementTagFull>> groupRefs = refs.get(parameterGroup);
        if (groupRefs == null) {
          groupRefs = new HashMap<String, ArrayList<PageElementTagFull>>();
          refs.put(parameterGroup, groupRefs);
        }

        // Analyze text reference
        String reference = ref.getText();
        if (reference != null) {
          reference = reference.trim();
        } else {
          reference = "";
        }
        if (reference.length() > 0) {
          ArrayList<PageElementTagFull> valueRefs = groupRefs.get(reference);
          if (valueRefs == null) {
            valueRefs = new ArrayList<PageElementTagFull>();
            groupRefs.put(reference, valueRefs);
          } else {
            if (errors == null) {
              return true;
            }
            result = true;
          }
          valueRefs.add(ref);
        }
      }
    }
    if (result == false) {
      return false;
    }

    // List of existing names
    /*ArrayList<Actionnable> existingNamesActions = new ArrayList<Actionnable>();
    for (Entry<String, HashMap<String, ArrayList<PageElementTag>>> groupRefsEntry : refs.entrySet()) {
      ArrayList<String> groupNames = new ArrayList<String>();
      HashMap<String, ArrayList<PageElementTag>> groupRefs = groupRefsEntry.getValue();
      for (Entry<String, ArrayList<PageElementTag>> valueRefsEntry : groupRefs.entrySet()) {
        ArrayList<PageElementTag> valueRefs = valueRefsEntry.getValue();
        for (PageElementTag ref : valueRefs) {
          String parameterName = ref.getParameter("name");
          if ((parameterName != null) && (parameterName.trim().length() > 0)) {
            if (!groupNames.contains(parameterName.trim())) {
              groupNames.add(parameterName.trim());
            }
          }
        }
      }
      if (groupNames.size() > 0) {
        Collections.sort(groupNames);
        ArrayList<Actionnable> groupActions = new ArrayList<Actionnable>(groupNames.size());
        for (String name : groupNames) {
          groupActions.add(new SimpleAction(name, new NoOpAction()));
        }
        existingNamesActions.add(new CompositeAction(groupRefsEntry.getKey(), groupActions));
      }
    }*/

    // Analyze ref tags
    for (Entry<String, HashMap<String, ArrayList<PageElementTagFull>>> groupRefsEntry : refs.entrySet()) {
      String groupName = groupRefsEntry.getKey();
      HashMap<String, ArrayList<PageElementTagFull>> groupRefs = groupRefsEntry.getValue();

      // Analyze duplicate ref tags
      for (Entry<String, ArrayList<PageElementTagFull>> valueRefsEntry : groupRefs.entrySet()) {
        String text = valueRefsEntry.getKey();
        ArrayList<PageElementTagFull> valueRefs = valueRefsEntry.getValue();
        if ((valueRefs != null) && (valueRefs.size() > 1)) {

          // Find possible names
          ArrayList<String> possibleNames = new ArrayList<String>();
          for (PageElementTagFull valueRef : valueRefs) {
            String parameterName = valueRef.getParameter("name");
            if ((parameterName != null) && (parameterName.trim().length() > 0)) {
              if (!possibleNames.contains(parameterName.trim())) {
                possibleNames.add(parameterName.trim());
              }
            }
          }

          if (possibleNames.size() > 0) {

            // Create an error for each tag, except for the first with the name
            boolean first = true;
            String correctName = possibleNames.get(0);
            for (PageElementTagFull valueRef : valueRefs) {
              String parameterName = valueRef.getParameter("name");
              if (parameterName != null) {
                parameterName = parameterName.trim();
              }
              boolean ok = first && correctName.equals(parameterName);
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(),
                  valueRef.getStartTagBeginIndex(), valueRef.getEndTagEndIndex(),
                  ok ?
                      CheckErrorResult.ErrorLevel.CORRECT :
                      CheckErrorResult.ErrorLevel.ERROR);
              if (!ok) {
                if (groupName.length() > 0) {
                  errorResult.addReplacement("<ref group=" + groupName + " name=" + correctName + "/>");
                } else {
                  errorResult.addReplacement("<ref name=" + correctName + "/>");
                }
              }
              /*if (existingNamesActions.size() > 0) {
                errorResult.addPossibleAction(
                    new CompositeAction(GT._("Existing references"), existingNamesActions));
              }*/
              errors.add(errorResult);
              if (ok) {
                first = false;
              }
            }
          } else {

            // Find if an URL is in the ref tag
            String url = null;
            if (text != null) {
              int httpIndex = -1;
              List<String> protocols = PageElementExternalLink.getProtocols();
              for (String protocol : protocols) {
                if (httpIndex < 0) {
                  httpIndex = text.indexOf(protocol);
                }
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

            // Create an error for each tag
            boolean first = true;
            for (PageElementTagFull valueRef : valueRefs) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(),
                  valueRef.getStartTagBeginIndex(), valueRef.getEndTagEndIndex(),
                  first ?
                      CheckErrorResult.ErrorLevel.WARNING :
                      CheckErrorResult.ErrorLevel.ERROR);

              // First ref tag : propose to give a name to it
              if (first) {
                errorResult.addPossibleAction(
                    GT._("Give a name to the <ref> tag"),
                    new AddTextActionProvider(
                        valueRef.getPartBeforeParameters() + " name=\"",
                        "\"" + valueRef.getPartFromParameters(),
                        new TextProviderUrlTitle(url),
                        GT._("What name would like to use for the <ref> tag ?"),
                        nameChecker));
              }

              /*if (existingNamesActions.size() > 0) {
                errorResult.addPossibleAction(
                    new CompositeAction(GT._("Existing references"), existingNamesActions));
              }*/
              if (url != null) {
                errorResult.addPossibleAction(new SimpleAction(
                    GT._("External Viewer"), new PageViewAction(url)));
              }
              errors.add(errorResult);
              first = false;
            }
          }
        }
      }
    }
    return true;
  }
}
