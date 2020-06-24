/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementPMID;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsComment;


/**
 * Algorithm for analyzing error 102 of check wikipedia project.
 * Error 102: PMID wrong syntax
 */
public class CheckErrorAlgorithm102 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm102() {
    super("PMID wrong syntax");
  }

  /** List of strings that could be before a PMID in <nowiki>. */
  private final static String[] EXTEND_BEFORE_NOWIKI = {
    "<nowiki>",
    "<small>",
    "(",
  };

  /** List of strings that could be after a PMID in <nowiki>. */
  private final static String[] EXTEND_AFTER_NOWIKI = {
    "</nowiki>",
    "</small>",
    ")",
  };

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

    // Analyze each PMID
    boolean result = false;
    List<PageElementPMID> pmids = analysis.getPMIDs();
    for (PageElementPMID pmid : pmids) {
      boolean isError = false;
      if (!pmid.isCorrect() && pmid.isValid()) {
        isError = true;
      }

      // Exclude special configured values for PMID
      /*if (isError && pmid.isTemplateParameter()) {
        WPCConfiguration config = analysis.getWPCConfiguration();
        List<String[]> specialValues = config.getStringArrayList(
            WPCConfigurationStringList.PMID_SPECIAL_VALUES);
        if ((specialValues != null) && !specialValues.isEmpty()) {
          PageElementTemplate template = analysis.isInTemplate(pmid.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(pmid.getBeginIndex());
            if ((param != null) &&
                (param.getName() != null) &&
                (param.getName().trim().length() > 0)) {
              String name = param.getName().trim();
              for (String[] specialValue : specialValues) {
                if ((specialValue.length > 2) &&
                    (Page.areSameTitle(template.getTemplateName(), specialValue[0])) &&
                    (name.equals(specialValue[1])) &&
                    (pmid.getPMIDNotTrimmed().equals(specialValue[2]))) {
                  isError = false;
                }
              }
            }
          }
        }
      }*/

      // Exclude parameters in templates
      if (isError &&
          pmid.isTemplateParameter() &&
          analysis.isInNamespace(Namespace.TEMPLATE)) {
        PageElementTemplate template = analysis.isInTemplate(pmid.getBeginIndex());
        if (template != null) {
          Parameter param = template.getParameterAtIndex(pmid.getBeginIndex());
          if (param != null) {
            List<PageElementFunction> functions = analysis.getFunctions();
            if (functions != null) {
              for (PageElementFunction function : functions) {
                int functionIndex = function.getBeginIndex();
                if ((template == analysis.isInTemplate(functionIndex)) &&
                    (param == template.getParameterAtIndex(functionIndex))) {
                  isError = false;
                }
              }
            }
          }
        }
      }

      // Report error
      boolean reported = false;
      if (isError) {
        if (errors == null) {
          return true;
        }
        result = true;
        reported = true;

        CheckErrorResult errorResult = createCheckErrorResult(analysis, pmid, false);
        errors.add(errorResult);
        List<String> replacements = pmid.getCorrectPMID();
        if (replacements != null) {
          for (String replacement : replacements) {
            if (!replacement.equals(analysis.getContents().substring(pmid.getBeginIndex(), pmid.getEndIndex()))) {
              errorResult.addReplacement(replacement);
            }
          }
        }
      }

      // Analyze if PMID is inside an external link
      if (!reported && !pmid.isTemplateParameter()) {
        PageElementExternalLink link = analysis.isInExternalLink(pmid.getBeginIndex());
        if ((link != null) && link.hasSquare() &&
            (pmid.getBeginIndex() >= link.getBeginIndex() + link.getTextOffset()) &&
            (link.getText() != null)) {
          if (errors == null) {
            return true;
          }
          result = true;
          reported = true;
          
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), link.getEndIndex());
          int beginIndex = pmid.getBeginIndex();
          int realEndIndex = pmid.getEndIndex();
          String contents = analysis.getContents();
          while ((beginIndex > 0) &&
                 (" ,;.(".indexOf(contents.charAt(beginIndex - 1)) >= 0)) {
            beginIndex--;
          }
          int endIndex = realEndIndex;
          while ((endIndex < link.getEndIndex()) &&
                 (")".indexOf(contents.charAt(endIndex)) >= 0)) {
            endIndex++;
          }
          if (beginIndex > link.getBeginIndex() + link.getTextOffset()) {
            String replacementPrefix =
                contents.substring(link.getBeginIndex(), beginIndex) +
                contents.substring(endIndex, link.getEndIndex()) +
                contents.substring(beginIndex, pmid.getBeginIndex());
            String textPrefix =
                contents.substring(link.getBeginIndex(), link.getBeginIndex() + 7) +
                "...]" +
                contents.substring(beginIndex, pmid.getBeginIndex());
            List<String> replacements = pmid.getCorrectPMID();
            for (String replacement : replacements) {
              errorResult.addReplacement(
                  replacementPrefix + replacement + contents.substring(realEndIndex, endIndex),
                  textPrefix + replacement + contents.substring(realEndIndex, endIndex));
            }
            errorResult.addReplacement(
                replacementPrefix + contents.substring(pmid.getBeginIndex(), pmid.getEndIndex()),
                textPrefix + contents.substring(pmid.getBeginIndex(), pmid.getEndIndex()));
            if (endIndex < link.getEndIndex()) {
              replacementPrefix =
                  contents.substring(link.getBeginIndex(), beginIndex) +
                  "]" +
                  contents.substring(beginIndex, pmid.getBeginIndex());
              for (String replacement : replacements) {
                errorResult.addReplacement(
                    replacementPrefix + replacement + contents.substring(pmid.getEndIndex(), link.getEndIndex() - 1),
                    textPrefix + replacement + contents.substring(pmid.getEndIndex(), link.getEndIndex() - 1));
              }
              errorResult.addReplacement(
                  replacementPrefix + contents.substring(pmid.getBeginIndex(), link.getEndIndex() - 1),
                  textPrefix + contents.substring(pmid.getBeginIndex(), link.getEndIndex() - 1));
            }
          } else if (endIndex >= link.getEndIndex() - 1) {
            List<String> replacements = pmid.getCorrectPMID();
            for (String replacement : replacements) {
              errorResult.addReplacement(replacement);
            }
            errorResult.addReplacement(contents.substring(pmid.getBeginIndex(), pmid.getEndIndex()));
          }
          errors.add(errorResult);
        }
      }
    }

    // Report also PMID inside <nowiki> tags
    List<PageElementTag> nowikiTags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_NOWIKI);
    if (nowikiTags != null) {
      String contents = analysis.getContents();
      for (PageElementTag nowikiTag : nowikiTags) {
        if (!nowikiTag.isFullTag() && nowikiTag.isComplete()) {
          String nowikiContent = contents.substring(
              nowikiTag.getValueBeginIndex(), nowikiTag.getValueEndIndex());
          int index = 0;
          while (index < nowikiContent.length()) {
            if (nowikiContent.startsWith(PageElementPMID.PMID_PREFIX, index)) {
              int tmpIndex = index + PageElementPMID.PMID_PREFIX.length();
              boolean hasSeparator = false;
              while ((tmpIndex < nowikiContent.length()) && 
                     (PageElementPMID.EXTRA_CHARACTERS.indexOf(nowikiContent.charAt(tmpIndex)) >= 0)) {
                hasSeparator = true;
                tmpIndex++;
              }
              boolean hasCharacter = false;
              int indexCharacter = tmpIndex;
              boolean shouldContinue = true;
              while (shouldContinue) {
                int tmpIndex2 = tmpIndex;
                shouldContinue = false;
                while ((tmpIndex2 < nowikiContent.length()) &&
                       (PageElementPMID.EXTRA_CHARACTERS.indexOf(nowikiContent.charAt(tmpIndex2)) >= 0)) {
                  tmpIndex2++;
                }
                while ((tmpIndex2 < nowikiContent.length()) &&
                       (PageElementPMID.POSSIBLE_CHARACTERS.indexOf(nowikiContent.charAt(tmpIndex2)) >= 0)) {
                  hasCharacter = true;
                  shouldContinue = true;
                  tmpIndex2++;
                }
                if (shouldContinue) {
                  tmpIndex = tmpIndex2;
                }
              }
              if (hasSeparator && hasCharacter) {
                if (errors == null) {
                  return true;
                }
                result = true;

                // Try to extend area
                int beginIndex = nowikiTag.getValueBeginIndex() + index;
                boolean extensionFound = false;
                do {
                  extensionFound = false;
                  for (String before : EXTEND_BEFORE_NOWIKI) {
                    if ((beginIndex >= before.length()) &&
                        (contents.startsWith(before, beginIndex - before.length()))) {
                      extensionFound = true;
                      beginIndex -= before.length();
                    }
                  }
                } while (extensionFound);
                int endIndex = nowikiTag.getValueBeginIndex() + tmpIndex;
                do {
                  extensionFound = false;
                  for (String after : EXTEND_AFTER_NOWIKI) {
                    if ((endIndex < contents.length()) &&
                        (contents.startsWith(after, endIndex))) {
                      extensionFound = true;
                      endIndex += after.length();
                    }
                  }
                } while (extensionFound);

                // Report error
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, beginIndex, endIndex);
                if ((beginIndex <= nowikiTag.getCompleteBeginIndex()) &&
                    (endIndex >= nowikiTag.getCompleteEndIndex())) {
                  errorResult.addReplacement(contents.substring(
                      nowikiTag.getValueBeginIndex() + index,
                      nowikiTag.getValueBeginIndex() + tmpIndex));
                  List<String[]> pmidTemplates = analysis.getWPCConfiguration().getStringArrayList(
                      WPCConfigurationStringList.PMID_TEMPLATES);
                  if (pmidTemplates != null) {
                    for (String[] pmidTemplate : pmidTemplates) {
                      if (pmidTemplate.length > 2) {
                        String templateName = pmidTemplate[0];
                        String[] params = pmidTemplate[1].split(",");
                        Boolean suggested = Boolean.valueOf(pmidTemplate[2]);
                        if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
                          StringBuilder replacement = new StringBuilder();
                          replacement.append("{{");
                          replacement.append(templateName);
                          replacement.append("|");
                          if (!"1".equals(params[0])) {
                            replacement.append(params[0]);
                            replacement.append("=");
                          }
                          replacement.append(nowikiContent.substring(indexCharacter, tmpIndex));
                          replacement.append("}}");
                          errorResult.addReplacement(replacement.toString());
                        }
                      }
                    }
                  }
                }
                errors.add(errorResult);
                index = tmpIndex;
              } else {
                index += PageElementPMID.PMID_PREFIX.length();
              }
            } else {
              index++;
            }
          }
        }
      }
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param pmid PMID.
   * @param checkForComment True to check for a comment after the PMID.
   * @return Error result.
   */
  protected CheckErrorResult createCheckErrorResult(
      PageAnalysis analysis, PageElementPMID pmid,
      boolean checkForComment) {
    ErrorLevel level = (pmid.isValid() && !pmid.helpRequested()) ?
        ErrorLevel.ERROR : ErrorLevel.WARNING;
    if (checkForComment) {
      String contents = analysis.getContents();
      int index = pmid.getEndIndex();
      while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
        index++;
      }
      if ((index < contents.length()) && (contents.charAt(index) == '<')) {
        ContentsComment comment = analysis.isInComment(index);
        if (comment != null) {
          level = ErrorLevel.WARNING;
        }
      }
    }
    CheckErrorResult result = createCheckErrorResult(
        analysis, pmid.getBeginIndex(), pmid.getEndIndex(), level);
    return result;
  }
}
