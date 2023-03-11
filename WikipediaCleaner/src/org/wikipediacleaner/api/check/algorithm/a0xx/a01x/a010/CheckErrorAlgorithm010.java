/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a01x.a010;

import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.api.data.contents.magicword.ImageMagicWordType;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 10 of check wikipedia project.
 * Error 10: Square brackets not correct end
 */
public class CheckErrorAlgorithm010 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm010() {
    super("Square brackets not correct end");
  }

  private final static String REJECTED_CHARS = "\n[{";

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

    // Analyze contents by looking for [[
    String contents = analysis.getContents();
    int maxLength = contents.length();
    int currentIndex = contents.indexOf("[[");
    boolean result = false;
    while (currentIndex >= 0) {
      boolean shouldCount = true;
      if (shouldCount) {
        PageElementInternalLink link = analysis.isInInternalLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementImage image = analysis.isInImage(currentIndex);
        if ((image != null) && (image.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementCategory category = analysis.isInCategory(currentIndex);
        if ((category != null) && (category.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementLanguageLink link = analysis.isInLanguageLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementInterwikiLink link = analysis.isInInterwikiLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementExternalLink link = analysis.isInExternalLink(currentIndex + 1);
        if ((link != null) && (link.getBeginIndex() == currentIndex + 1)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if (analysis.comments().isAt(currentIndex) ||
            (analysis.getSurroundingTag(WikiTagType.NOWIKI, currentIndex) != null) ||
            (analysis.getSurroundingTag(HtmlTagType.CODE, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.MAPFRAME, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.MATH, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.MATH_CHEM, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.PRE, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.SCORE, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.SOURCE, currentIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, currentIndex) != null) ||
            (analysis.isInTag(currentIndex) != null)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementTemplate template = analysis.isInTemplate(currentIndex + 2);
        if ((template != null) && (contents.startsWith("]]", template.getEndIndex()))) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if (errors == null) {
          return true;
        }
        result = true;
        
        // Check if there is a potential end
        int tmpIndex = currentIndex + 2;
        boolean errorReported = false;
        boolean finished = false;
        while (!finished && (tmpIndex < maxLength)) {
          char tmpChar = contents.charAt(tmpIndex);
          if (REJECTED_CHARS.indexOf(tmpChar) >= 0) {
            finished = true;
          } else if (tmpChar == ']') {
            int tmpIndex2 = tmpIndex + 1;
            while ((tmpIndex2 < maxLength) &&
                   (contents.charAt(tmpIndex2) != ']') &&
                   (REJECTED_CHARS.indexOf(contents.charAt(tmpIndex2)) < 0)) {
              tmpIndex2++;
            }
            String suffix = "";
            if ((tmpIndex2 < maxLength) && (contents.charAt(tmpIndex2) == ']')) {
              suffix = contents.substring(tmpIndex + 1, tmpIndex2);
              while (suffix.endsWith(" ") &&
                     (tmpIndex2 + 1 < maxLength) &&
                     (contents.charAt(tmpIndex2) + 1 == ' ')) {
                suffix = suffix.substring(0, suffix.length() - 1);
              }
            } else {
              tmpIndex2 = tmpIndex;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, tmpIndex2 + 1);

            // Check if the situation is something like [[http://....] (replacement: [http://....])
            boolean protocolFound = PageElementExternalLink.isPossibleProtocol(contents, currentIndex + 2);
            if (protocolFound) {
              errorResult.addReplacement(contents.substring(currentIndex + 1, tmpIndex2 + 1));
            }

            boolean automatic = false;
            int lineBeginIndex = ContentsUtil.getLineBeginIndex(contents, currentIndex);
            int lineEndIndex = ContentsUtil.getLineEndIndex(contents, currentIndex);
            if ((lineBeginIndex == currentIndex) &&
                (lineEndIndex == tmpIndex2  + 1)) {
              int colonIndex = contents.indexOf(':', currentIndex + 2);
              if ((colonIndex > currentIndex + 2) && (colonIndex < lineEndIndex)) {
                int namespace = Namespace.getNamespace(
                    analysis.getWikiConfiguration().getNamespaces(),
                    contents.substring(currentIndex + 2, colonIndex));
                if (namespace == Namespace.CATEGORY) {
                  int openBracketIndex = contents.indexOf('[', currentIndex + 2);
                  int closeBracketIndex = contents.indexOf(']', currentIndex + 2);
                  if ((closeBracketIndex == tmpIndex2) &&
                      ((openBracketIndex < 0) || (openBracketIndex > lineEndIndex))) {
                    automatic = true;
                  }
                }
              }
              // TODO: analyze if category at the beginning of a line
            }
            errorResult.addReplacement(
                contents.substring(currentIndex, tmpIndex) + "]]" + suffix,
                automatic && (suffix.length() == 0));
            if (suffix.length() > 0) {
              errorResult.addReplacement(contents.substring(currentIndex, tmpIndex) + suffix + "]]");
            }
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          } else if (tmpChar == '}') {
            int lastChar = tmpIndex;
            if ((lastChar + 1 < maxLength) && (contents.charAt(lastChar + 1) == '}')) {
              lastChar++;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, lastChar + 1);
            errorResult.addReplacement(contents.substring(currentIndex, tmpIndex) + "]]");
            errorResult.addReplacement("{{" + contents.substring(currentIndex + 2, tmpIndex) + "}}");
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          }
          tmpIndex++;
        }

        // Default
        if (!errorReported) {

          // Check if there's an internal link just after
          int extraBrackets = 0;
          if (contents.startsWith("[[", currentIndex + 1)) {
            PageElement nextLink = analysis.isInInternalLink(currentIndex + 2);
            if (nextLink == null) {
              nextLink = analysis.isInCategory(currentIndex + 2);
            }
            if (nextLink != null) {
              if (nextLink.getBeginIndex() == currentIndex + 2) {
                extraBrackets = 2;
              } else if (nextLink.getBeginIndex() == currentIndex + 1) {
                extraBrackets = 1;
              }
            }
          }

          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, currentIndex, currentIndex + 2 + extraBrackets);
          errorResult.addReplacement("[[", extraBrackets == 2);
          errors.add(errorResult);
        }
      }
      currentIndex = contents.indexOf("[[", currentIndex + 2);
    }

    // Analyze each internal link to see if it contains a [
    result |= analyzeInternalLinks(analysis, errors);

    // Analyze each image to see if it contains a [
    for (PageElementImage image : analysis.getImages()) {
      String modifiedText = null;
      String text = StringUtils.EMPTY;
      PageElementImage.Parameter descriptionParam = image.getDescriptionParameter();
      if (descriptionParam != null) {
        text = descriptionParam.getContents();
        modifiedText = cleanImageText(
            text, analysis,
            image.getBeginIndex() + descriptionParam.getBeginOffset());
      }
      String modifiedAlt = null;
      String alt = StringUtils.EMPTY;
      PageElementImage.Parameter altParam = image.getParameter(ImageMagicWordType.IMG_ALT);
      if (altParam != null) {
        int equalIndex = altParam.getContents().indexOf('=');
        if (equalIndex > 0) {
          alt = altParam.getContents().substring(equalIndex + 1);
          modifiedAlt = cleanImageText(
              alt, analysis,
              image.getBeginIndex() + altParam.getBeginOffset() + equalIndex + 1);
        }
      }
      if ((modifiedText != null) || (modifiedAlt != null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, image.getBeginIndex(), image.getEndIndex());
        errorResult.addReplacement(image.getDescriptionReplacement(
            (modifiedText != null) ? modifiedText : text,
            (modifiedAlt != null) ? modifiedAlt : alt));
        errors.add(errorResult);
      }
    }

    // Analyze each external link to see if it has a [ before
    for (PageElementExternalLink link : analysis.getExternalLinks()) {
      int begin = link.getBeginIndex();
      if (link.hasSquare()) {
        if ((begin > 0) && (contents.charAt(begin - 1) == '[')) {
          int end = link.getEndIndex();
          if ((end >= contents.length()) || (contents.charAt(end) != ']')) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, begin - 1, begin);
            errorResult.addReplacement("[");
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * Analyze internal links to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementInternalLink link : analysis.getInternalLinks()) {
      if (link.getText() != null) {
        int singleBracketsCount = 0;
        int firstOpeningBracket = -1;
        int extraClosingBracket = -1;
        int currentIndex = link.getBeginIndex() + link.getTextOffset() - 1;
        while (true) {
          currentIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, currentIndex + 1, "[]<");
          if (currentIndex >= link.getEndIndex() - 2) {
            break;
          }
          char currentChar = contents.charAt(currentIndex);
          if (currentChar == '[') {
            if (singleBracketsCount == 0) {
              firstOpeningBracket = currentIndex;
            }
            singleBracketsCount++;
          } else if (currentChar == ']') {
            if (singleBracketsCount > 0) {
              singleBracketsCount--;
            } else {
              extraClosingBracket = currentIndex;
            }
          } else if (currentChar == '<') {
            PageElementTag tag = analysis.isInTag(currentIndex, WikiTagType.NOWIKI);
            if ((tag != null) && (tag.getBeginIndex() == currentIndex) &&
                (tag.isComplete()) &&
                (tag.isFullTag() || !tag.isEndTag())) {
              currentIndex = tag.getCompleteEndIndex() - 1;
            }
          }
        }
        if ((singleBracketsCount > 0) || (extraClosingBracket >= 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          int remove = (singleBracketsCount > 0) ? firstOpeningBracket : extraClosingBracket;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, link.getBeginIndex(), link.getEndIndex());
          String text =
              contents.substring(link.getBeginIndex() + link.getTextOffset(), remove) +
              contents.substring(remove, link.getEndIndex() - 2);
          errorResult.addReplacement(InternalLinkBuilder
              .from(link.getLink())
              .withAnchor(link.getAnchor())
              .withText(text)
              .toString());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * @param originalText Original text to be cleaned.
   * @param analysis Page analysis.
   * @param offset Offset of the original text in the full text.
   * @return Cleaned up text (or null if no cleanup required).
   */
  private String cleanImageText(String originalText, PageAnalysis analysis, int offset) {
    if (originalText == null) {
      return null;
    }
    StringBuilder sb = null;
    int index = 0;
    int singleBracketsCount = 0;
    while (index < originalText.length()) {
      boolean done = false;

      // Handle templates inside the text
      if (!done && originalText.startsWith("{{", index)) {
        PageElementTemplate template = analysis.isInTemplate(offset + index);
        if ((template != null) &&
            (template.getBeginIndex() == offset + index) &&
            (template.getEndIndex() - offset <= originalText.length())) {
          index = template.getEndIndex() - offset;
          done = true;
        }
      }

      // Handle tags inside the text
      if (!done && originalText.startsWith("<", index)) {
        PageElementTag tag = analysis.isInTag(offset + index);
        if ((tag != null) &&
            (tag.getBeginIndex() == offset + index)) {
          if (tag.isComplete() &&
              (tag.getCompleteEndIndex() - offset <= originalText.length()) &&
              (WikiTagType.MATH.equals(tag.getType()) ||
               WikiTagType.NOWIKI.equals(tag.getType()))) {
            index = tag.getCompleteEndIndex() - offset;
            done = true;
          } else {
            if (tag.getEndIndex() - offset <= originalText.length()) {
              index = tag.getEndIndex() - offset;
              done = true;
            }
          }
        }
      }

      // Handle double opening brackets or other situations except single opening bracket
      if (!done) {
        boolean doubleBrackets = originalText.startsWith("[[", index);
        if (doubleBrackets || !originalText.startsWith("[", index)) {
          boolean  ok = true;
          if (originalText.startsWith("]", index)) {
            doubleBrackets = originalText.startsWith("]]", index);
            if (!doubleBrackets) {
              if (singleBracketsCount > 0) {
                singleBracketsCount--;
              } else {
                ok = false;
                if (sb == null) {
                  sb = new StringBuilder(originalText.substring(0, index));
                }
              }
            }
          }
          int count = doubleBrackets ? 2 : 1;
          if (ok && (sb != null)) {
            sb.append(originalText.substring(index, index + count));
          }
          index += count;
          done = true;
        }
      }

      // Handle single opening bracket
      if (!done) {
        singleBracketsCount++;
        boolean paired = false;
        int index2 = index + 1;
        while (!paired && (index2 < originalText.length())) {
          if (originalText.startsWith("]", index2) && !originalText.startsWith("]]", index2)) {
            paired = true;
          }
          index2++;
        }
        if (!paired) {
          if (sb == null) {
            sb = new StringBuilder(originalText.substring(0, index));
          }
        } else if (sb != null) {
          sb.append('[');
        }
        index++;
      }
    }
    return (sb != null) ? sb.toString() : null;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
