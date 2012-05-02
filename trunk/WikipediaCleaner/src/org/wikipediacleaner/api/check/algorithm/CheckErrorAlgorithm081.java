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
import java.util.Map;
import java.util.Map.Entry;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerReferenceName;
import org.wikipediacleaner.utils.TextProvider;
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

    // Group tags by group and value for further analyze
    List<PageElementTag> completeRefTags =
        pageAnalysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    String contents = pageAnalysis.getContents();
    boolean result = false;
    Map<String, Map<String, List<PageElementTag>>> refs =
        new HashMap<String, Map<String, List<PageElementTag>>>();
    for (PageElementTag tag : completeRefTags) {
      int valueBeginIndex = tag.getValueBeginIndex();
      int valueEndIndex = tag.getValueEndIndex();
      if ((!tag.isFullTag()) &&
          (valueBeginIndex > 0) &&
          (valueEndIndex > 0) &&
          (valueBeginIndex < valueEndIndex)) {

        // Retrieve references with the same group name
        Parameter group = tag.getParameter("group");
        String groupName = (group != null) ? group.getValue() : null;
        Map<String, List<PageElementTag>> groupRefs = refs.get(groupName);
        if (groupRefs == null) {
          groupRefs = new HashMap<String, List<PageElementTag>>();
          refs.put(groupName, groupRefs);
        }

        // Retrieve references with the same text
        String text = contents.substring(valueBeginIndex, valueEndIndex).trim();
        if (text.length() > 0) {
          List<PageElementTag> valueRefs = groupRefs.get(text);
          if (valueRefs == null) {
            valueRefs = new ArrayList<PageElementTag>();
            groupRefs.put(text, valueRefs);
          }
          if (errors == null) {
            return true;
          }
          result = true;
          valueRefs.add(tag);
        }
      }
    }
    if (result == false) {
      return false;
    }

    // Second pass for managing with several tags having the same group and value
    for (Entry<String, Map<String, List<PageElementTag>>> entryGroup : refs.entrySet()) {
      String groupName = entryGroup.getKey();
      for (Entry<String, List<PageElementTag>> entryValue : entryGroup.getValue().entrySet()) {
        List<PageElementTag> listTags = entryValue.getValue();
        if (listTags.size() > 1) {

          // List possible names
          List<String> possibleNames = new ArrayList<String>();
          for (PageElementTag tag : listTags) {
            Parameter name = tag.getParameter("name");
            if ((name != null) && (name.getValue() != null)) {
              String nameValue = name.getValue().trim();
              if ((nameValue.length() > 0) && (!possibleNames.contains(nameValue))) {
                possibleNames.add(nameValue);
              }
            }
          }

          if (possibleNames.size() > 0) {

            // Create an error for each tag, except for the first with the name
            String selectedName = possibleNames.get(0);
            boolean first = true;
            for (PageElementTag tag : listTags) {
              Parameter name = tag.getParameter("name");
              String nameValue = (name != null) ? name.getValue() : null;
              if (nameValue != null) {
                nameValue = nameValue.trim();
              }
              boolean ok = first && (selectedName.equals(nameValue));
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(),
                  tag.getCompleteBeginIndex(), tag.getCompleteEndIndex(),
                  ok ? CheckErrorResult.ErrorLevel.CORRECT : CheckErrorResult.ErrorLevel.ERROR);
              if (!ok) {
                if ((groupName != null) && (groupName.length() > 0)) {
                  errorResult.addReplacement(
                      "<ref group=\"" + groupName + "\" name=\"" + selectedName + "\"/>");
                } else {
                  errorResult.addReplacement("<ref name=\"" + selectedName + "\"/>");
                }
              } else {
                first = false;
              }
              errors.add(errorResult);
            }
          } else {

            for (PageElementTag tag : listTags) {
              int valueBeginIndex = tag.getValueBeginIndex();
              int valueEndIndex = tag.getValueEndIndex();

              // Find if an external link is in the reference tag
              List<PageElementExternalLink> externalLinks = pageAnalysis.getExternalLinks();
              List<PageElementExternalLink> links = new ArrayList<PageElementExternalLink>();
              for (PageElementExternalLink externalLink : externalLinks) {
                if ((externalLink.getBeginIndex() >= valueBeginIndex) &&
                    (externalLink.getEndIndex() <= valueEndIndex)) {
                  links.add(externalLink);
                }
              }

              // Register error
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(),
                  tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());

              // Add an action for naming the reference tag
              // TODO: manage a better action for naming the reference tag and replacing all other tags
              TextProvider provider = null;
              if (links.size() > 0) {
                provider = new TextProviderUrlTitle(links.get(0).getLink());
              }
              String prefix = contents.substring(tag.getBeginIndex(), tag.getEndIndex() - 1);
              String suffix = contents.substring(tag.getEndIndex() - 1, tag.getCompleteEndIndex());
              errorResult.addPossibleAction(
                  GT._("Give a name to the <ref> tag"),
                  new AddTextActionProvider(
                      prefix + " name=\"",
                      "\"" + suffix,
                      provider,
                      GT._("What name would like to use for the <ref> tag ?"),
                      nameChecker));

              // Add actions for external links
              for (PageElementExternalLink link : links) {
                errorResult.addPossibleAction(new SimpleAction(
                    GT._("External Viewer"),
                    new PageViewAction(link.getLink())));
              }
              errors.add(errorResult);
            }
          }
        }
      }
    }
    return true;
  }
}
