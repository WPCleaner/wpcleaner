/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JOptionPane;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.CompleteTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
    GT._T("Fix reference duplication"),
  };

  public CheckErrorAlgorithm081() {
    super("Reference duplication");
    nameChecker = new StringCheckerReferenceName();
  }

  /**
   * Group tags by group and value.
   * 
   * @param analysis Page analysis.
   * @param refs Tags (out).
   * @return True if there are several tags with the same text.
   */
  private boolean groupTags(
      PageAnalysis analysis,
      Map<String, Map<String, List<PageElementTag>>> refs) {
    List<PageElementTag> completeRefTags =
        analysis.getCompleteTags(WikiTagType.REF);
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTag tag : completeRefTags) {
      int valueBeginIndex = tag.getValueBeginIndex();
      int valueEndIndex = tag.getValueEndIndex();
      if ((!tag.isFullTag()) &&
          (valueBeginIndex > 0) &&
          (valueEndIndex > 0) &&
          (valueBeginIndex < valueEndIndex)) {

        // Retrieve references with the same group name
        String groupName = tag.getGroupOfRef(analysis);
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
      if ((name != null) && (name.getTrimmedValue() != null)) {
        String nameValue = name.getTrimmedValue();
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
    CompleteTagBuilder builder = CompleteTagBuilder.from(WikiTagType.REF, StringUtils.trim(value));
    if ((groupName != null) && (groupName.trim().length() > 0)) {
      builder.addAttribute("group", groupName.trim());
    }
    if ((tagName != null) && (tagName.trim().length() > 0)) {
      builder.addAttribute("name", tagName.trim());
    }
    return builder.toString();
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
    if (analysis == null) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Group tags by group and value for further analyze
    Map<String, Map<String, List<PageElementTag>>> refs =
        new HashMap<String, Map<String, List<PageElementTag>>>();
    boolean result = groupTags(analysis, refs);
    if (result == false) {
      return false;
    } else if (errors == null) {
      return true;
    }

    // Second pass for managing with several tags having the same group and value
    List<PageElementTag> completeReferencesTags =
        analysis.getCompleteTags(WikiTagType.REFERENCES);
    String contents = analysis.getContents();
    for (Entry<String, Map<String, List<PageElementTag>>> entryGroup : refs.entrySet()) {
      String groupName = entryGroup.getKey();
      for (Entry<String, List<PageElementTag>> entryValue : entryGroup.getValue().entrySet()) {
        List<PageElementTag> listTags = entryValue.getValue();
        if (listTags.size() > 1) {

          // Find main reference tag
          PageElementTag mainTag = PageElementTag.getMainRef(
              listTags, completeReferencesTags, analysis);
          if (mainTag != null) {

            // Create an error for each tag, except for the main tag
            String selectedName = mainTag.getParameter("name").getTrimmedValue();
            for (PageElementTag tag : listTags) {
              if (tag == mainTag) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis,
                    tag.getCompleteBeginIndex(), tag.getCompleteEndIndex(),
                    CheckErrorResult.ErrorLevel.CORRECT);
                errors.add(errorResult);
              } else {
                Parameter name = tag.getParameter("name");
                String nameValue = (name != null) ? name.getTrimmedValue() : null;
                if (nameValue != null) {
                  nameValue = nameValue.trim();
                }
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis,
                    tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
                errorResult.addReplacement(
                    getClosedRefTag(groupName, selectedName, null),
                    selectedName.equals(nameValue) || (name == null));
                errors.add(errorResult);
              }
            }
          } else {

            for (PageElementTag tag : listTags) {
              int valueBeginIndex = tag.getValueBeginIndex();
              int valueEndIndex = tag.getValueEndIndex();

              // Find if an external link is in the reference tag
              List<PageElementExternalLink> externalLinks = analysis.getExternalLinks();
              List<PageElementExternalLink> links = new ArrayList<PageElementExternalLink>();
              for (PageElementExternalLink externalLink : externalLinks) {
                if ((externalLink.getBeginIndex() >= valueBeginIndex) &&
                    (externalLink.getEndIndex() <= valueEndIndex)) {
                  links.add(externalLink);
                }
              }

              // Register error
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis,
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
                  GT._T("Give a name to the <ref> tag"),
                  new AddTextActionProvider(
                      prefix + " name=\"",
                      "\"" + suffix,
                      provider,
                      GT._T("What name would you like to use for the <ref> tag ?"),
                      nameChecker));

              // Add actions for external links
              for (PageElementExternalLink link : links) {
                errorResult.addPossibleAction(new SimpleAction(
                    GT._T("External Viewer"),
                    new ActionExternalViewer(link.getLink())));
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
    Map<String, Map<String, List<PageElementTag>>> refsByGroupAndValue =
        new HashMap<String, Map<String, List<PageElementTag>>>();
    groupTags(analysis, refsByGroupAndValue);

    // Memorize tag names by group and value
    Map<String, Map<String, List<String>>> refNamesByGroupAndValue =
        new HashMap<String, Map<String, List<String>>>();
    for (Entry<String, Map<String, List<PageElementTag>>> refsByValueForGroup : refsByGroupAndValue.entrySet()) {
      String groupName = refsByValueForGroup.getKey();
      Map<String, List<PageElementTag>> groupRefsByValue = refsByValueForGroup.getValue();
      Map<String, List<String>> groupRefNamesByValue = new HashMap<String, List<String>>();
      refNamesByGroupAndValue.put(groupName, groupRefNamesByValue);
      for (Entry<String, List<PageElementTag>> refsForGroupAndValue : groupRefsByValue.entrySet()) {
        String value = refsForGroupAndValue.getKey();
        List<PageElementTag> refTags = refsForGroupAndValue.getValue();
        List<String> possibleNames = getRefNames(refTags);
        groupRefNamesByValue.put(value, possibleNames);
      }
    }

    // Check all reference tags
    List<PageElementTag> completeReferencesTags = analysis.getCompleteTags(WikiTagType.REFERENCES);
    List<PageElementTag> completeRefTags = analysis.getCompleteTags(WikiTagType.REF);
    Object highlight = null;
    String contents = analysis.getContents();
    for (PageElementTag refTag : completeRefTags) {

      // Retrieve basic information
      PageElementTag.Parameter groupParameter = refTag.getParameter("group");
      String groupName = (groupParameter != null) ? groupParameter.getValue() : null;
      Map<String, List<PageElementTag>> groupRefs = refsByGroupAndValue.get(groupName);
      String valueRef = contents.substring(refTag.getValueBeginIndex(), refTag.getValueEndIndex());
      List<PageElementTag> valueRefs = groupRefs.get(valueRef);

      // Check if there is more than one tag with the same value
      if ((valueRefs != null) && (valueRefs.size() > 1)) {

        // Display selection
        highlight = addHighlight(
            textPane, refTag.getCompleteBeginIndex(), refTag.getCompleteEndIndex());
        textPane.select(refTag.getCompleteBeginIndex(), refTag.getCompleteEndIndex());
  
        // Check if a name already exists
        String replacement = null;
        List<String> possibleNames = refNamesByGroupAndValue.get(groupName).get(valueRef);
        if ((possibleNames != null) && (possibleNames.size() > 0)) {
          String selectedName = possibleNames.get(0);
          PageElementTag mainRef = PageElementTag.getMainRef(
              valueRefs, completeReferencesTags, analysis);
          if (mainRef != refTag) {
            String tmp = getClosedRefTag(groupName, selectedName, null);
            String message =
                GT._T("A <ref> tag shares the same content, and is named \"{0}\".", selectedName) + "\n" +
                GT._T("Do you want to replace this <ref> tag by \"{0}\" ?", tmp);
            int answer = Utilities.displayYesNoCancelWarning(textPane.getParent(), message);
            if (answer == JOptionPane.YES_OPTION) {
              replacement = tmp;
            } else if (answer == JOptionPane.CANCEL_OPTION) {
              break;
            }
          }
        } else {
          String message =
              GT._T("Several <ref> tags share the same content, but none has been given a name.") +"\n" +
              GT._T("What name do you want to use for this <ref> tag ?");
          String newText = Utilities.askForValue(textPane.getParent(), message, (String) null, nameChecker);
          if (newText != null) {
            replacement = getClosedRefTag(groupName, newText, valueRef);
            possibleNames = Collections.singletonList(newText);
            refNamesByGroupAndValue.get(groupName).put(valueRef, possibleNames);
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

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    // TODO: move to initializeSettings()
    List<String[]> refTemplates = analysis.getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.REFERENCES_TEMPLATES);
    if ((refTemplates == null) ||
        (refTemplates.isEmpty()) ||
        analysis.getPage().isInUserNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
