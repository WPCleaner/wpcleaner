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

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Utilities methods for Page.
 */
public class PageUtilities {

  /**
   * Count occurences of links in the text.
   * 
   * @param wikipedia Wikipedia.
   * @param page Currently analyzed page.
   * @param pageText Page text.
   * @param link Link counted.
   */
  public static void countLinkOccurencesInText(
      EnumWikipedia wikipedia, Page page,
      String pageText, Page link) {
    int count = 0;

    // Look for disambiguation links
    Pattern pattern = createPatternForInternalLink(link);
    Matcher matcher = pattern.matcher(pageText);
    while (matcher.find()) {
      count++;
    }

    // Analyze templates
    for (int numT = 0; numT < wikipedia.getDisambiguationMatchesCount(); numT++) {
      TemplateMatch template = wikipedia.getDisambiguationMatch(numT);
      if (!template.isGood()) {
        pattern = createPatternForTemplate(template);
        matcher = pattern.matcher(pageText);
        while (matcher.find()) {
          ArrayList<TemplateParameter> parameters = analyzeTemplateParameters(template, matcher, page);
          if (parameters != null) {
            for (TemplateParameter param : parameters) {
              // Count page
              if (param.isRelevant() && link.getTitle().equals(param.getValue())) {
                count++;
              }
            }
          }
        }
      }
    }

    link.setCountOccurence(count);
  }

  /**
   * Creates a Pattern for matching internal links to give <code>page</code>.
   * 
   * @param link The interesting page.
   * @return Pattern.
   */
  public static Pattern createPatternForInternalLink(Page link) {
    if (link == null) {
      return null;
    }
    String title = link.getTitle();

    // Create the regular expression
    StringBuilder expression = new StringBuilder();
    expression.append("\\[\\["); // [[
    expression.append("\\:?"); // Possible :
    expression.append("(?:" + link.getWikipedia().getCode() + "\\:)?"); // Possible <lang>:
    expression.append("(");
    addPatternForWhiteSpaces(expression);
    addPatternForTitle(expression, title);
    addPatternForWhiteSpaces(expression);
    expression.append("(\\|([^\\|\\]]*))?"); // Possible text
    // TODO: Check if possessive quantifier could speed up pattern matching.
    expression.append(")\\]\\]"); // ]]
    //System.err.println("Regular expression: " + expression.toString());
    Pattern pattern = Pattern.compile(expression.toString());
    return pattern;
  }

  /**
   * Creates a Pattern for matching templates.
   * 
   * @param template The interesting template.
   * @return Pattern.
   */
  public static Pattern createPatternForTemplate(TemplateMatch template) {
    if (template == null) {
      return null;
    }
    String title = template.getName();

    // Create the regular expression
    StringBuilder expression = new StringBuilder();
    expression.append("\\{\\{"); // {{
    expression.append("(");
    addPatternForWhiteSpaces(expression);
    addPatternForTitle(expression, title);
    addPatternForWhiteSpaces(expression);
    expression.append("(?:\\|(" +
                        "(?:" +
                          "(?:[^\\{\\}]" + "*+" + ")" + // Parameters text
                          "|" +
                          "(?:\\{\\{\\!\\}\\})" + // Special {{!}}
                        ")*?" +
                      "))?"); // Possible parameters
    expression.append(")\\}\\}"); // }}
    Pattern pattern = Pattern.compile(expression.toString());
    return pattern;
  }

  /**
   * Retrieve template parameters.
   * 
   * @param template Template matcher.
   * @param matcher Current matcher.
   * @param page Page.
   * @return Template parameters.
   */
  public static ArrayList<TemplateParameter> analyzeTemplateParameters(
      TemplateMatch template, Matcher matcher, Page page) {
    String text = null;
    if (matcher.group(matcher.groupCount()) != null) {
      text = matcher.group(matcher.groupCount());
    } else {
      text = template.getDefaultParameters();
    }
    return analyzeTemplateParameters(template, text, page);
  }

  /**
   * Retrieve template parameters.
   * 
   * @param template Template matcher.
   * @param text Text.
   * @param page Page.
   * @return Template parameters.
   */
  public static ArrayList<TemplateParameter> analyzeTemplateParameters(
      TemplateMatch template, String text, Page page) {
    String[] parameters = text.split("\\|");
    if ((parameters == null) || (parameters.length == 0)) {
      return null;
    }
    ArrayList<TemplateParameter> result = new ArrayList<TemplateParameter>(parameters.length);
    int currentParam = 0;
    for (int param = 0; param < parameters.length; param++) {
      // Analyze each parameter
      if (page != null) {
        parameters[param] = parameters[param].replaceAll("\\{\\{PAGENAME\\}\\}", page.getTitleUcFirst());
        parameters[param] = parameters[param].replaceAll("\\{\\{pagename\\}\\}", page.getTitleLcFirst());
      }
      String[] tokens = parameters[param].split("=");
      String paramName = null;
      String paramValue = null;
      if (tokens.length == 1) {
        currentParam++;
        paramName = Integer.toString(currentParam);
        paramValue = tokens[0];
      } else {
        paramName = tokens[0];
        paramValue = tokens[1];
      }
      TemplateParameter templateParam = new TemplateParameter(paramName, paramValue); 
      for (int p = 0; p < template.getParametersCount(); p++) {
        if (paramName.equals(template.getParameter(p))) {
          templateParam.setRelevant(true);
        }
      }
      result.add(templateParam);
    }
    return result;
  }

  /**
   * @param expression Pattern being created.
   * @param title Page title.
   */
  public static void addPatternForTitle(StringBuilder expression, String title) {
    if ((title == null) || (title.length() == 0)) {
      return;
    }
    int begin = 0;
    while (begin < title.length()) {
      int space = title.indexOf(' ', begin);
      if (space < 0) {
        space = title.length();
      }
      if (begin == 0) {
        expression.append("[");
        expression.append(Character.toLowerCase(title.charAt(0)));
        expression.append(Character.toUpperCase(title.charAt(0)));
        expression.append("]");
        expression.append(Pattern.quote(title.substring(1, space)));
      } else {
        expression.append(Pattern.quote(title.substring(begin, space)));
      }
      while ((space < title.length()) && (title.charAt(space) == ' ')) {
        expression.append("[ _]");
        space++;
      }
      begin = space;
    }
  }

  /**
   * @param expression Pattern being created.
   */
  public static void addPatternForWhiteSpaces(StringBuilder expression) {
    expression.append("\\s*+"); // Possible white characters
    // Note: the possessive quantifier '+' is being used to find all white spaces
  }
}
