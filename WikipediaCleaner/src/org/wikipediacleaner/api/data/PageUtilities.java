/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Utilities methods for Page.
 */
public class PageUtilities {

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
  public static List<TemplateParameter> analyzeTemplateParameters(
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
  public static List<TemplateParameter> analyzeTemplateParameters(
      TemplateMatch template, String text, Page page) {
    String[] parameters = text.split("\\|");
    if ((parameters == null) || (parameters.length == 0)) {
      return null;
    }
    List<TemplateParameter> result = new ArrayList<>(parameters.length);
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
