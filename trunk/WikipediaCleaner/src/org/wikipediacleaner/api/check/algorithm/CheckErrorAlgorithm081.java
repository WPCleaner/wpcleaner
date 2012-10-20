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

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
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

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix reference duplication"),
  };

  public CheckErrorAlgorithm081() {
    super("Reference duplication");
    nameChecker = new StringCheckerReferenceName();
  }

  /**
   * Group tags by group and value.
   * 
   * @param pageAnalysis Page analysis.
   * @param refs Tags (out).
   * @return True if there are several tags with the same text.
   */
  private boolean groupTags(
      PageAnalysis pageAnalysis,
      Map<String, Map<String, List<PageElementTag>>> refs) {
    List<PageElementTag> completeRefTags =
        pageAnalysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    String contents = pageAnalysis.getContents();
    boolean result = false;
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
          if (valueRefs.size() > 0) {
            result = true;
          }
          valueRefs.add(tag);
        }
      }
    }
    return result;
  }

  /**
   * Get all names used in a list of reference tags.
   *  
   * @param refs List of reference tags.
   * @return List of names.
   */
  private List<String> getRefNames(List<PageElementTag> refs) {
    List<String> possibleNames = new ArrayList<String>();
    for (PageElementTag tag : refs) {
      Parameter name = tag.getParameter("name");
      if ((name != null) && (name.getValue() != null)) {
        String nameValue = name.getValue().trim();
        if ((nameValue.length() > 0) && (!possibleNames.contains(nameValue))) {
          possibleNames.add(nameValue);
        }
      }
    }
    return possibleNames;
  }

  /**
   * Construct a closed reference tag.
   * 
   * @param groupName Name of the group.
   * @param tagName Name of the tag.
   * @param value Value of the tag.
   * @return Reference tag.
   */
  private String getClosedRefTag(String groupName, String tagName, String value) {
    StringBuilder result = new StringBuilder();
    result.append("<ref");
    if ((groupName != null) && (groupName.trim().length() > 0)) {
      result.append(" group=\"");
      result.append(groupName.trim());
      result.append("\"");
    }
    if ((tagName != null) && (tagName.trim().length() > 0)) {
      result.append(" name=\"");
      result.append(tagName.trim());
      result.append("\"");
    }
    if ((value != null) && (value.trim().length() > 0)) {
      result.append(">");
      result.append(value.trim());
      result.append("</ref>");
    } else {
      result.append("/>");
    }
    return result.toString();
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
    Map<String, Map<String, List<PageElementTag>>> refs =
        new HashMap<String, Map<String, List<PageElementTag>>>();
    boolean result = groupTags(pageAnalysis, refs);
    if (result == false) {
      return false;
    } else if (errors == null) {
      return true;
    }

    // Second pass for managing with several tags having the same group and value
    String contents = pageAnalysis.getContents();
    for (Entry<String, Map<String, List<PageElementTag>>> entryGroup : refs.entrySet()) {
      String groupName = entryGroup.getKey();
      for (Entry<String, List<PageElementTag>> entryValue : entryGroup.getValue().entrySet()) {
        List<PageElementTag> listTags = entryValue.getValue();
        if (listTags.size() > 1) {

          // List possible names
          List<String> possibleNames = getRefNames(listTags);
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
                errorResult.addReplacement(getClosedRefTag(groupName, selectedName, null));
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

    // Initialize
    StringBuilder tmpContents = new StringBuilder();
    int currentIndex = 0;

    // Group tags by group and value for further analyze
    Map<String, Map<String, List<PageElementTag>>> refs =
        new HashMap<String, Map<String, List<PageElementTag>>>();
    groupTags(analysis, refs);

    // Memorize tag names by group and value
    Map<String, Map<String, String>> refNames =
        new HashMap<String, Map<String,String>>();
    for (Entry<String, Map<String, List<PageElementTag>>> group : refs.entrySet()) {
      String groupName = group.getKey();
      Map<String, List<PageElementTag>> groupRefs = group.getValue();
      Map<String, String> mapNames = new HashMap<String, String>();
      refNames.put(groupName, mapNames);
      for (Entry<String, List<PageElementTag>> valueRefs : groupRefs.entrySet()) {
        String value = valueRefs.getKey();
        List<PageElementTag> refTags = valueRefs.getValue();
        List<String> possibleNames = getRefNames(refTags);
        if ((possibleNames != null) && (possibleNames.size() > 0)) {
          mapNames.put(value, possibleNames.get(0));
        }
      }
    }

    // Check all reference tags
    List<PageElementTag> completeRefTags =
        analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    Object highlight = null;
    String contents = analysis.getContents();
    for (PageElementTag refTag : completeRefTags) {

      // Retrieve basic information
      PageElementTag.Parameter groupParameter = refTag.getParameter("group");
      String groupName = (groupParameter != null) ? groupParameter.getValue() : null;
      Map<String, List<PageElementTag>> groupRefs = refs.get(groupName);
      String valueRef = contents.substring(refTag.getValueBeginIndex(), refTag.getValueEndIndex());
      List<PageElementTag> valueRefs = groupRefs.get(valueRef);

      // Check if there is more than one tag with the same value
      if (valueRefs.size() > 1) {

        // Display selection
        highlight = addHighlight(
            textPane, refTag.getCompleteBeginIndex(), refTag.getCompleteEndIndex());
        textPane.select(refTag.getCompleteBeginIndex(), refTag.getCompleteEndIndex());
  
        // Check if a name already exists
        String replacement = null;
        String selectedName = refNames.get(groupName).get(valueRef);
        if (selectedName != null) {
          PageElementTag firstRef = null;
          for (PageElementTag tmpRef : valueRefs) {
            if ((firstRef == null) && (selectedName.equals(tmpRef.getName()))) {
              firstRef = tmpRef;
            }
          }
          if (firstRef != refTag) {
            String tmp = getClosedRefTag(groupName, selectedName, null);
            String message =
                GT._("A <ref> tag shares the same content, and is named \"{0}\".", selectedName) + "\n" +
                GT._("Do you want to replace this <ref> tag by \"{0}\" ?", tmp);
            int answer = Utilities.displayYesNoCancelWarning(textPane.getParent(), message);
            if (answer == JOptionPane.YES_OPTION) {
              replacement = tmp;
            } else if (answer == JOptionPane.CANCEL_OPTION) {
              break;
            }
          }
        } else {
          String message =
              GT._("Several <ref> tags share the same content, but none has been given a name.") +"\n" +
              GT._("What name do you want to use for this <ref> tag ?");
          String newText = Utilities.askForValue(textPane.getParent(), message, (String) null, nameChecker);
          if (newText != null) {
            replacement = getClosedRefTag(groupName, newText, valueRef);
            refNames.get(groupName).put(valueRef, newText);
          }
        }

        // Do the replacement
        if (replacement != null) {
          if (currentIndex < refTag.getBeginIndex()) {
            tmpContents.append(contents.substring(currentIndex, refTag.getCompleteBeginIndex()));
          }
          tmpContents.append(replacement);
          currentIndex = refTag.getCompleteEndIndex();
        }
        removeHighlight(textPane, highlight);
        highlight = null;
      }
    }
    removeHighlight(textPane, highlight);
    highlight = null;

    // Return result
    if (currentIndex == 0) {
      return contents;
    }
    if (currentIndex < contents.length()) {
      tmpContents.append(contents.substring(currentIndex));
    }
    return tmpContents.toString();
  }
}
