/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;


/**
 * Class containing information about a RFC (Requests for Comments).
 */
public class PageElementRFC extends PageElement {

  /** RFC prefix */
  public final static String RFC_PREFIX = "RFC";

  /** RFC incorrect prefixes */
  private final static String[] RFC_INCORRECT_PREFIX = {
    "rfc"
  };

  /** Possible first characters for the prefix */
  private final static String POSSIBLE_FIRST_CHARACTERS_PREFIX = "Rr";

  /** RFC possible meaningful characters */
  private final static String POSSIBLE_CHARACTERS = "0123456789";

  /** RFC possible extraneous characters */
  private final static String EXTRA_CHARACTERS = "";

  /** RFC incorrect characters */
  private final static String INCORRECT_CHARACTERS = "-:‐\t—=–\n";

  /** RFC incorrect characters at the beginning */
  private final static String INCORRECT_BEGIN_CHARACTERS = "-:‐\t—=–\n";

  /**
   * @param analysis Page analysis.
   * @return List of RFC.
   */
  public static List<PageElementRFC> analyzePage(
      PageAnalysis analysis) {
    List<PageElementRFC> rfcs = new ArrayList<PageElementRFC>();

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> rfcIgnoreTemplates = config.getStringArrayList(WPCConfigurationStringList.RFC_IGNORE_TEMPLATES); 

    // Search for RFC templates
    List<String[]> rfcTemplates = config.getStringArrayList(WPCConfigurationStringList.RFC_TEMPLATES);
    if (rfcTemplates != null) {
      for (String[] rfcTemplate : rfcTemplates) {
        if (rfcTemplate.length > 0) {
          String[] params = null;
          List<PageElementTemplate> templates = analysis.getTemplates(rfcTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              if (params == null) {
                if (rfcTemplate.length > 1) {
                  params = rfcTemplate[1].split(",");
                } else {
                  params = new String[]{ "1" };
                }
              }
              for (String param : params) {
                if ((param != null) && (param.length() > 0)) {
                  analyzeTemplateParams(
                      analysis, rfcs, rfcIgnoreTemplates,
                      template, param,
                      false, false, false, false);
                }
              }
            }
          }
        }
      }
    }

    // Search for RFC templates where help is requested
    rfcTemplates = config.getStringArrayList(WPCConfigurationStringList.RFC_HELP_NEEDED_TEMPLATES);
    if (rfcTemplates != null) {
      for (String[] rfcTemplate : rfcTemplates) {
        if (rfcTemplate.length > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(rfcTemplate[0]);
          if (templates != null) {
            for (PageElementTemplate template : templates) {
              analyzeTemplateParams(
                  analysis, rfcs, rfcIgnoreTemplates,
                  template,
                  ((rfcTemplate.length > 1) && (rfcTemplate[1].length() > 0)) ? rfcTemplate[1] : "1",
                  false, false, false, true);
            }
          }
        }
      }
    }

    // Search for RFC in template parameters
    //List<PageElementTemplate> templates = analysis.getTemplates();
    //for (PageElementTemplate template : templates) {
    //  analyzeTemplateParams(
    //      analysis, rfcs, rfcIgnoreTemplates,
    //      template, "RFC", true, true, true, false);
    //}

    // Search for RFC in plain texts
    analyzePlainText(analysis, rfcs);

    return rfcs;
  }

  /**
   * @param index Current index.
   * @param rfcs List of RFC.
   * @return True if the current index is already in a RFC.
   */
  private static boolean isInRFC(int index, List<PageElementRFC> rfcs) {
    if (rfcs != null) {
      for (PageElementRFC tmpRfc : rfcs) {
        if ((tmpRfc.getBeginIndex() <= index) &&
            (tmpRfc.getEndIndex() > index)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Analyze plain text for RFC.
   * 
   * @param analysis Page analysis.
   * @param rfcs Current list of RFC.
   */
  private static void analyzePlainText(
      PageAnalysis analysis, List<PageElementRFC> rfcs) {
    String contents = analysis.getContents();
    if (contents == null) {
      return;
    }
    int index = 0;
    int maxIndex = contents.length() - 1;
    while (index < maxIndex) {
      index = checkPlainText(analysis, contents, index, rfcs);
    }
  }

  /**
   * Check plain text for RFC.
   * 
   * @param analysis Page analysis.
   * @param contents Page contents.
   * @param index Current index in the page.
   * @param rfcs Current list of RFC.
   * @return Next index to check.
   */
  private static int checkPlainText(
      PageAnalysis analysis, String contents, int index, List<PageElementRFC> rfcs) {

    // Check special places
    char currentChar = contents.charAt(index);
    if (currentChar == '<') {
      ContentsComment comment = analysis.comments().getAt(index);
      if (comment != null) {
        return comment.getEndIndex();
      }
      PageElementTag tag = analysis.isInTag(index);
      if (tag != null) {
        String tagName = tag.getName();
        if (PageElementTag.TAG_WIKI_NOWIKI.equals(tagName) ||
            PageElementTag.TAG_WIKI_PRE.equals(tagName) ||
            PageElementTag.TAG_WIKI_SOURCE.equals(tagName) ||
            PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT.equals(tagName)) {
          return tag.getCompleteEndIndex();
        }
        return tag.getEndIndex();
      }
    }
    if (currentChar == '[') {
      PageElementInterwikiLink iwLink = analysis.isInInterwikiLink(index);
      if ((iwLink != null) && (iwLink.getBeginIndex() == index)) {
        return iwLink.getEndIndex();
      }
    }

    // Check if it's a potential RFC
    if (POSSIBLE_FIRST_CHARACTERS_PREFIX.indexOf(currentChar) < 0) {
      return index + 1;
    }
    String prefix = null;
    boolean correct = false;
    if (contents.startsWith(RFC_PREFIX, index)) {
      prefix = RFC_PREFIX;
      correct = true;
    }
    for (String tmpPrefix : RFC_INCORRECT_PREFIX) {
      if ((prefix == null) &&
          (contents.length() >= index + tmpPrefix.length()) &&
          ContentsUtil.startsWithIgnoreCase(contents, tmpPrefix, index)) {
        prefix = tmpPrefix;
        correct = false;
      }
    }
    if (prefix == null) {
      return index + 1;
    }

    // Manage specific locations
    if (isInRFC(index, rfcs)) {
      return index + 1;
    }
    PageElementTemplate template = analysis.isInTemplate(index);
    if (template != null) {
      if (template.getParameterCount() == 0) {
        return template.getEndIndex();
      }
      int pipeIndex = template.getParameterPipeIndex(0);
      if (index < pipeIndex) {
        return pipeIndex + 1;
      }
    }
    PageElementImage image = analysis.isInImage(index);
    if (image != null) {
      int pipeIndex = image.getBeginIndex() + image.getFirstPipeOffset();
      if (index < pipeIndex) {
        return pipeIndex + 1;
      }
    }
    boolean isValid = true;
    PageElementExternalLink link = analysis.isInExternalLink(index);
    if (link != null) {
      if (!link.hasSquare() ||
          (index < link.getBeginIndex() + link.getTextOffset()) ||
          (link.getText() == null)) {
        isValid = false;
      }
    }

    // Check if it's a template parameter
    boolean parameter = false;
    if (template != null) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        if ((template.getParameterPipeIndex(paramNum) < index) &&
            (template.getParameterValueStartIndex(paramNum) > index)) {
          parameter = true;
        }
      }
    }

    int beginIndex = index;
    index += prefix.length();
    boolean isCorrect = correct;
    if (!parameter) {
      if (beginIndex >= 2) {
        if (contents.startsWith("[[", beginIndex - 2)) {
          isCorrect = false;
          beginIndex -= 2;
          if ((index + 2 < contents.length()) && contents.startsWith("]]", index)) {
            index += 2;
          }
        }
      }
      boolean spaceFound = false;
      PageElementInternalLink iLink = null;
      PageElementExternalLink eLink = null;
      if (!analysis.comments().isAt(index)) {
        boolean done = false;
        while (!done) {
          done = true;
          if (index < contents.length()) {
            currentChar = contents.charAt(index);
            if (currentChar == ' ') {
              index++;
              spaceFound = true;
              done = false;
            } else if (currentChar == '[') {
              iLink = analysis.isInInternalLink(index);
              if ((iLink != null) && (iLink.getBeginIndex() == index)) {
                isCorrect = false;
                if (iLink.getTextOffset() > 0) {
                  index += iLink.getTextOffset();
                } else {
                  index += 2;
                }
              } else {
                eLink = analysis.isInExternalLink(index);
                if ((eLink != null) && (eLink.getBeginIndex() == index)) {
                  isCorrect = false;
                  if (eLink.getTextOffset() > 0) {
                    index += eLink.getTextOffset();
                  } else {
                    index += 1;
                  }
                }
              }
            } else if (contents.startsWith("&nbsp;", index)) {
              index += "&nbsp;".length();
              spaceFound = true;
              done = false;
            } else if (INCORRECT_BEGIN_CHARACTERS.indexOf(currentChar) >= 0) {
              index++;
              isCorrect = false;
              done = false;
            }
          }
        }
      }
      int beginNumber = -1;
      int endNumber = beginNumber;
      boolean finished = false;
      isCorrect &= spaceFound;
      boolean nextCorrect = isCorrect;
      while (!finished && (index < contents.length())) {
        currentChar = contents.charAt(index);
        if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
          if (beginNumber < 0) {
            beginNumber = index;
          }
          endNumber = index + 1;
          index++;
          isCorrect = nextCorrect;
        } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
          if (beginNumber < 0) {
            nextCorrect = false;
          }
          index++;
        } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
          index++;
          nextCorrect = false;
        } else {
          if ((endNumber == index) && (Character.isLetter(currentChar))) {
            isCorrect = false;
          }
          finished = true;
        }
      }
      if (endNumber > beginNumber) {
        String number = contents.substring(beginNumber, endNumber);
        if ((iLink != null) && (endNumber + 2 == iLink.getEndIndex())) {
          endNumber = iLink.getEndIndex();
        } else if ((eLink != null) && (endNumber + 1 == eLink.getEndIndex())) {
          endNumber = eLink.getEndIndex();
        } else if (contents.startsWith("[[", beginIndex) &&
                   contents.startsWith("]]", endNumber)) {
          endNumber += 2;
        }
        rfcs.add(new PageElementRFC(
            beginIndex, endNumber, analysis, number,
            isValid, isCorrect, false, null));
        index = endNumber;
      } else {
        if (contents.startsWith(prefix, index) &&
            !contents.startsWith("[[RFC#", beginIndex)) {
          rfcs.add(new PageElementRFC(
              beginIndex, index, analysis, "",
              isValid, false, false, null));
        }
      }
    }

    return index;
  }

  /**
   * Check if template parameter is a RFC.
   * 
   * @param analysis Page analysis.
   * @param rfcs Current list of RFC.
   * @param ignoreTemplates List of templates (with parameter and value) to ignore.
   * @param template Template.
   * @param argumentName Template parameter name.
   * @param ignoreCase True if parameter name should compared ignoring case.
   * @param acceptNumbers True if numbers are accepted after parameter name.
   * @param acceptAllValues True if all values are accepted, even if not compatible with RFC. 
   * @param helpRequested True if help has been requested for this RFC.
   */
  private static void analyzeTemplateParams(
      PageAnalysis analysis, List<PageElementRFC> rfcs,
      List<String[]> ignoreTemplates,
      PageElementTemplate template,
      String argumentName,
      boolean ignoreCase, boolean acceptNumbers,
      boolean acceptAllValues, boolean helpRequested) {

    // Check if template should be ignored
    if (ignoreTemplates != null) {
      for (String[] ignoreTemplate : ignoreTemplates) {
        if ((ignoreTemplate != null) &&
            (ignoreTemplate.length > 0) &&
            (Page.areSameTitle(ignoreTemplate[0], template.getTemplateName()))) {
          if (ignoreTemplate.length > 1) {
            String paramValue = template.getParameterValue(ignoreTemplate[1]);
            if (ignoreTemplate.length > 2) {
              if ((paramValue != null) &&
                  (paramValue.trim().equals(ignoreTemplate[2].trim()))) {
                return; // Ignore all templates with this name and parameter set to a given value
              }
            } else {
              if (paramValue != null) {
                return; // Ignore all templates with this name and parameter present
              }
            }
          } else {
            return; // Ignore all templates with this name
          }
        }
      }
    }

    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {

      // Check parameter name
      Parameter param = template.getParameter(paramNum);
      String paramName = param.getComputedName();
      boolean nameOk = false;
      if ((ignoreCase && argumentName.equalsIgnoreCase(paramName)) ||
          (argumentName.equals(paramName))) {
        nameOk = true;
      } else if (acceptNumbers && (paramName.length() > argumentName.length())) {
        String shortParamName = paramName.substring(0, argumentName.length());
        if ((ignoreCase && argumentName.equalsIgnoreCase(shortParamName)) ||
            (argumentName.equals(paramName))) {
          nameOk = true;
          for (int i = argumentName.length(); i < paramName.length(); i++) {
            if (!Character.isDigit(paramName.charAt(i))) {
              nameOk = false;
            }
          }
        }
      }
      
      // Parameter is for a RFC, analyze its value
      if (nameOk) {
        String paramValue = param.getStrippedValue();
        boolean ok = true;
        boolean hasDigit = false;
        int i = 0;
        int beginIndex = -1;
        int endIndex = -1;
        boolean correct = true;
        while (ok && (i < paramValue.length())) {
          char currentChar = paramValue.charAt(i);
          if (POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) {
            if (Character.isDigit(currentChar)) {
              if (beginIndex < 0) {
                beginIndex = i;
              }
              endIndex = i + 1;
              hasDigit = true;
            } else if (Character.toUpperCase(currentChar) == 'X') {
              endIndex = i + 1;
            }
            i++;
          } else if (EXTRA_CHARACTERS.indexOf(currentChar) >= 0) {
            i++;
          } else if (INCORRECT_CHARACTERS.indexOf(currentChar) >= 0) {
            i++;
            correct = false;
          } else {
            ok = false;
          }
        }
        int delta = template.getParameterValueStartIndex(paramNum);
        if (beginIndex < 0) {
          beginIndex = 0;
        }
        beginIndex += delta;
        if (endIndex < 0) {
          endIndex = 0;
        }
        endIndex += delta;
        if (beginIndex < 0) {
          ok = false;
        } else {
          if (!ok && hasDigit && (paramValue.charAt(i) == '<')) {
            ContentsComment comment = analysis.comments().getBeginsAt(beginIndex + i);
            if (comment != null) {
              ok = true;
              i += comment.getEndIndex() - comment.getBeginIndex();
              while (ok && (i < paramValue.length())) {
                char currentChar = paramValue.charAt(i);
                if (currentChar == '<') {
                  comment = analysis.comments().getAt(beginIndex + i);
                  if (comment != null) {
                    i += comment.getEndIndex() - comment.getBeginIndex();
                  } else {
                    ok = false;
                  }
                } else if ((currentChar != ' ') && (currentChar != '\n')) {
                  ok = false;
                } else {
                  i++;
                }
              }
            }
          }
        }
        if (ok) {
          String value = analysis.getContents().substring(beginIndex, endIndex);
          if (paramValue.length() > 0) {
            rfcs.add(new PageElementRFC(
                beginIndex, endIndex, analysis, value,
                true, correct, helpRequested, template));
          }
        } else if (acceptAllValues) {
          if (paramValue.length() > 0) {
            rfcs.add(new PageElementRFC(
                template.getParameterValueStartIndex(paramNum),
                template.getParameterValueStartIndex(paramNum) + paramValue.length(),
                analysis, paramValue, true, false, false, template));
          }
        }
      }
    }
  }

  /** WPCleaner configuration */
  private final WPCConfiguration wpcConfiguration;

  /** Full text */
  private final String fullText;

  /** RFC not trimmed */
  private final String rfcNotTrimmed;

  /** RFC (trimmed) */
  private final String rfc;

  /** True if RFC is in a valid location */
  private final boolean isValid;

  /** True if RFC syntax is correct */
  private final boolean isCorrect;

  /** Template if RFC is a template parameter (RFC=...) */
  private final PageElementTemplate template;

  /** True if help has been requested for this RFC */
  private final boolean helpRequested;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param analysis Page analysis.
   * @param rfc RFC.
   * @param isValid True if RFC is in a valid location.
   * @param isCorrect True if RFC syntax is correct.
   * @param helpRequested True if help has been requested for this RFC. 
   * @param template Template if ISBN is a template parameter.
   */
  private PageElementRFC(
      int beginIndex, int endIndex, PageAnalysis analysis,
      String rfc, boolean isValid,
      boolean isCorrect, boolean helpRequested,
      PageElementTemplate template) {
    super(beginIndex, endIndex);
    this.wpcConfiguration = analysis.getWPCConfiguration();
    this.fullText = analysis.getContents().substring(beginIndex, endIndex);
    this.rfcNotTrimmed = rfc;
    this.rfc = cleanRFC(rfc);
    this.isValid = isValid;
    this.isCorrect = isCorrect;
    this.helpRequested = helpRequested;
    this.template = template;
  }

  /**
   * @return RFC not trimmed.
   */
  public String getRFCNotTrimmed() {
    return rfcNotTrimmed;
  }

  /**
   * @return RFC (trimmed).
   */
  public String getRFC() {
    return rfc;
  }

  /**
   * @return URL to see the RFC.
   */
  public String getURL() {
    return "https://tools.ietf.org/html/rfc" + rfc;
  }

  /**
   * @return True if RFC is in a valid location.
   */
  public boolean isValid() {
    return isValid;
  }

  /**
   * @return True if RFC syntax is correct.
   */
  public boolean isCorrect() {
    return isCorrect;
  }

  /**
   * @return True if help has been requested for this RFC.
   */
  public boolean helpRequested() {
    return helpRequested;
  }

  /**
   * @return True if RFC is a template parameter.
   */
  public boolean isTemplateParameter() {
    return (template != null);
  }

  /**
   * @return List of possible RFC.
   */
  public List<String> getCorrectRFC() {
    List<String> result = new ArrayList<String>();
    String prefix = isTemplateParameter() ? "" : "RFC ";

    // Prefix outside the template
    if ((template != null) &&
        (getBeginIndex() < template.getBeginIndex())) {
      if (fullText != null) {
        result.add(fullText.substring(template.getBeginIndex() - getBeginIndex()));
      }
      return result;
    }

    // Construct a basic RFC number
    StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < rfcNotTrimmed.length(); i++) {
      char currentChar = rfcNotTrimmed.charAt(i);
      if ((POSSIBLE_CHARACTERS.indexOf(currentChar) >= 0) ||
          (EXTRA_CHARACTERS.indexOf(currentChar) >= 0)) {
        buffer.append(currentChar);
      } else if (currentChar == '\t') {
        buffer.append(" ");
      } else {
        buffer.append(currentChar);
      }
    }
    String cleanedRFC = buffer.toString().trim();

    // Basic replacement
    addCorrectRFC(result, prefix, cleanedRFC);
    
    return result;
  }

  /**
   * @param result List of possible replacements.
   * @param prefix RFC prefix.
   * @param cleanedRFC Cleaned up RFC.
   */
  private void addCorrectRFC(List<String> result, String prefix, String cleanedRFC) {
    addCorrectRFC(result, prefix + cleanedRFC);
    if (!isTemplateParameter()) {
      List<String[]> rfcTemplates = wpcConfiguration.getStringArrayList(
          WPCConfigurationStringList.RFC_TEMPLATES);
      if (rfcTemplates != null) {
        for (String[] rfcTemplate : rfcTemplates) {
          if (rfcTemplate.length > 2) {
            String[] params = rfcTemplate[1].split(",");
            Boolean suggested = Boolean.valueOf(rfcTemplate[2]);
            if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
              TemplateBuilder builder = TemplateBuilder.from(rfcTemplate[0]);
              builder.addParam(
                  !"1".equals(params[0]) ? params[0] : null,
                  cleanedRFC);
              addCorrectRFC(result, builder.toString());
            }
          }
        }
      }
      
    }
  }

  /**
   * @param result List of possible replacements.
   * @param correctRFC Possible replacement.
   */
  private void addCorrectRFC(List<String> result, String correctRFC) {
    if ((result == null) || (correctRFC == null)) {
      return;
    }
    if (!result.contains(correctRFC)) {
      result.add(correctRFC);
    }
  }

  /**
   * @param helpNeededTemplate Name of template for asking for help.
   * @param reason Reason of the request.
   * @return Text for requesting for help.
   */
  public String askForHelp(
      String[] helpNeededTemplate, String reason) {
    if ((helpNeededTemplate == null) ||
        (helpNeededTemplate.length == 0)) {
      return null;
    }
    if (isTemplateParameter()) {
      return null;
    }

    // Template name
    TemplateBuilder builder = TemplateBuilder.from(helpNeededTemplate[0]);

    // ISBN
    builder.addParam(
        (helpNeededTemplate.length > 1) ? helpNeededTemplate[1] : null,
        getRFCNotTrimmed());

    // Reason
    if ((reason != null) &&
        (helpNeededTemplate.length > 2) &&
        (helpNeededTemplate[2].length() > 0)) {
      builder.addParam(helpNeededTemplate[2], reason);
    }

    // Extra parameters
    for (int i = 3; i < helpNeededTemplate.length; i++) {
      builder.addParam(helpNeededTemplate[i]);
    }

    return builder.toString();
  }

  /**
   * @param comment Comment for asking for help.
   * @param reason Reason of the request.
   * @return Text for requesting for help.
   */
  public String askForHelp(
      String comment, String reason) {
    if ((comment == null) ||
        (comment.trim().length() == 0)) {
      return null;
    }
    StringBuilder replacement = new StringBuilder();
    replacement.append(comment);
    if ((reason != null) && (reason.trim().length() > 0)) {
      replacement.append(" - ");
      replacement.append(reason);
    }
    return CommentBuilder.from(replacement.toString()).toString();
  }

  /**
   * @param rfc RFC number.
   * @return Cleaned up RFC number.
   */
  public static String cleanRFC(String rfc) {
    if (rfc == null) {
      return null;
    }
    rfc = rfc.trim();
    if (rfc.length() == 0) {
      return rfc;
    }
    PageAnalysis analysis = new PageAnalysis(null, rfc);
    StringBuilder result = new StringBuilder();
    int i = 0;
    while (i < rfc.length()) {
      char current = Character.toUpperCase(rfc.charAt(i));
      if (current == '<') {
        ContentsComment comment = analysis.comments().getBeginsAt(i);
        if (comment != null) {
          i = comment.getEndIndex() - 1;
        } else {
          PageElementTag refTag = analysis.isInTag(i, PageElementTag.TAG_WIKI_REF);
          if ((refTag != null) && (refTag.getBeginIndex() == i)) {
            i = refTag.getCompleteEndIndex() - 1;
          }
        }
      } else if (POSSIBLE_CHARACTERS.indexOf(current) >= 0) {
        result.append(current);
      }
      i++;
    }
    return result.toString();
  }

  /**
   * @param rfcValue RFC value.
   * @return True if RFC value is valid.
   */
  public static boolean isValid(String rfcValue) {
    if (rfcValue == null) {
      return false;
    }
    rfcValue = cleanRFC(rfcValue);
    if (rfcValue.length() == 0) {
      return false;
    }
    return true;
  }
}
