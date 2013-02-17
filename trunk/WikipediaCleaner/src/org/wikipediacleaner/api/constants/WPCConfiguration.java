/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.api.constants;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.PageAnalysisUtils;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Suggestion;
import org.wikipediacleaner.api.data.TemplateMatch;
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.api.data.TemplateMatcher1L;
import org.wikipediacleaner.api.data.TemplateMatcher1L2T;
import org.wikipediacleaner.api.data.TemplateMatcher1LT;
import org.wikipediacleaner.i18n.GT;


/**
 * Configuration for WPCleaner.
 */
public class WPCConfiguration {

  /**
   * Logger.
   */
  private final static Log log = LogFactory.getLog(WPCConfiguration.class);

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * Constructor for WPCleaner configuration.
   * 
   * @param wiki Wiki.
   */
  public WPCConfiguration(EnumWikipedia wiki) {
    this.wiki = wiki;
    generalBooleanValues = new HashMap<WPCConfigurationBoolean, Boolean>();
    userBooleanValues = new HashMap<WPCConfigurationBoolean, Boolean>();
    generalStringValues = new HashMap<WPCConfigurationString, String>();
    userStringValues = new HashMap<WPCConfigurationString, String>();
    generalStringListValues = new HashMap<WPCConfigurationStringList, List<String>>();
    userStringListValues = new HashMap<WPCConfigurationStringList, List<String>>();
    generalLongValues = new HashMap<WPCConfigurationLong, Long>();
    userLongValues = new HashMap<WPCConfigurationLong, Long>();
    initDefaultEncyclopedicNamespaces();
  }

  /**
   * @param input Reader for general WPCleaner configuration.
   */
  public void setGeneralConfiguration(Reader input) throws APIException {
    cleanConfiguration();
    BufferedReader reader = new BufferedReader(input);
    while (setNextParameter(reader, true)) {
      //
    }
    try {
      reader.close();
    } catch (IOException e) {
      // Nothing
    }
  }

  /**
   * Clean configuration.
   */
  private void cleanConfiguration() {
    generalBooleanValues.clear();
    userBooleanValues.clear();
    generalStringValues.clear();
    userStringValues.clear();
    generalStringListValues.clear();
    userStringListValues.clear();
    generalLongValues.clear();
    userLongValues.clear();
    initDefaultEncyclopedicNamespaces();
    disambiguationCategories = null;
    suggestions = null;
    templateMatchers = new HashMap<String, List<TemplateMatcher>>();
    templatesAfterAskHelp = null;
    wiktionaryMatches = null;
  }

  /**
   * @param input Reader for user WPCleaner configuration.
   */
  public void setUserConfiguration(Reader input) throws APIException {
    BufferedReader reader = new BufferedReader(input);
    while (setNextParameter(reader, false)) {
      //
    }
    try {
      reader.close();
    } catch (IOException e) {
      // Nothing
    }
  }

  /**
   * Extract next parameter from WPCleaner configuration.
   *
   * @param reader Reader for the configuration.
   * @param general Flag indicating if dealing with general or user properties.
   * @return Next parameter found.
   * @throw APIException.
   */
  private boolean setNextParameter(
      BufferedReader reader, boolean general) throws APIException {
    String line;
    try {
      while ((line = reader.readLine()) != null) {
        int posEqual = line.indexOf('=');
        boolean nameOk = true;
        for (int i = 0; i < posEqual; i++) {
          if (!Character.isLetterOrDigit(line.charAt(i)) &&
              (line.charAt(i) != '_') &&
              (line.charAt(i) != ' ')) {
            nameOk = false;
          }
        }
        if ((posEqual > 0) && nameOk) {
          String name = line.substring(0, posEqual);
          line = line.substring(posEqual + 1);
          int posEnd = line.indexOf(" END");
          while ((posEnd == -1) && (!"END".equals(line))) {
            String nextLine = reader.readLine();
            if (nextLine != null) {
              line += "\n" + nextLine;
              posEnd = line.indexOf(" END");
            } else {
              posEnd = line.length();
            }
          }
          line = line.substring(0, posEnd);
          setProperty(name, line, general);
          return true;
        }
      }
    } catch (IOException e) {
      throw new APIException("Error reading WPCleaner configuration: " + e.getMessage());
    }
    return false;
  }

  /**
   * Map for holding boolean values for general settings.
   */
  private final Map<WPCConfigurationBoolean, Boolean> generalBooleanValues;

  /**
   * Map for holding boolean values for user settings.
   */
  private final Map<WPCConfigurationBoolean, Boolean> userBooleanValues;

  /**
   * Map for holding String values for general settings.
   */
  private final Map<WPCConfigurationString, String> generalStringValues;

  /**
   * Map for holding String values for user settings.
   */
  private final Map<WPCConfigurationString, String> userStringValues;

  /**
   * Map for holding String list values for general settings.
   */
  private final Map<WPCConfigurationStringList, List<String>> generalStringListValues;

  /**
   * Map for holding String list values for user settings.
   */
  private final Map<WPCConfigurationStringList, List<String>> userStringListValues;

  /**
   * Map for holding Long values for general settings.
   */
  private final Map<WPCConfigurationLong, Long> generalLongValues;

  /**
   * Map for holding Long values for user settings.
   */
  private final Map<WPCConfigurationLong, Long> userLongValues;

  /**
   * Retrieve the value of a Boolean attribute.
   * 
   * @param attribute Attribute.
   * @return Attribute value.
   */
  public boolean getBoolean(WPCConfigurationBoolean attribute) {
    Boolean result = userBooleanValues.get(attribute);
    if (result == null) {
      result = generalBooleanValues.get(attribute);
    }
    if (result == null) {
      result = Boolean.valueOf(attribute.getDefaultValue());
    }
    return result.booleanValue();
  }

  /**
   * Retrieve the value of a String attribute.
   * 
   * @param attribute Attribute.
   * @return Attribute value.
   */
  public String getString(WPCConfigurationString attribute) {
    String result = userStringValues.get(attribute);
    if ((result == null) ||
        ((!attribute.canBeEmpty()) && (result.trim().length() == 0))) {
      result = generalStringValues.get(attribute);
    }
    if ((result == null) ||
        ((!attribute.canBeEmpty()) && (result.trim().length() == 0))) {
      result = attribute.getDefaultValue();
    }
    if ((result != null) && (!attribute.canBeEmpty()) && (result.trim().length() == 0)) {
      result = null;
    }
    return result;
  }

  /**
   * Retrieve the value of a String list attribute.
   * 
   * @param attribute Attribute.
   * @return Attribute value.
   */
  public List<String> getStringList(WPCConfigurationStringList attribute) {
    List<String> userResult = userStringListValues.get(attribute);
    List<String> generalResult = generalStringListValues.get(attribute);
    if ((userResult == null) && (generalResult == null)) {
      return null;
    }
    List<String> result = new ArrayList<String>();
    if ((generalResult != null) && ((userResult == null) || (attribute.canCombine()))) {
      result.addAll(generalResult);
    }
    if (userResult != null) {
      result.addAll(userResult);
    }
    return result;
  }

  /**
   * Retrieve the value of a Long attribute.
   * 
   * @param attribute Attribute.
   * @return Attribute value.
   */
  public long getLong(WPCConfigurationLong attribute) {
    Long result = userLongValues.get(attribute);
    if (result == null) {
      result = generalLongValues.get(attribute);
    }
    if (result == null) {
      result = Long.valueOf(attribute.getDefaultValue());
    }
    return result.longValue();
  }

  /**
   * Set property.
   * 
   * @param name Property name.
   * @param value Property value.
   * @param general Flag indicating if dealing with general or user properties.
   */
  private void setProperty(
      String name, String value, boolean general) {
    if (name == null) {
      return;
    }
    name = name.trim();
    if (value != null) {
      value = value.trim();
    }

    // Check if it is a Boolean attribute
    WPCConfigurationBoolean booleanAttribute = WPCConfigurationBoolean.getValue(name);
    if (booleanAttribute != null) {
      Boolean booleanValue = Boolean.valueOf(value);
      if (general) {
        if (booleanAttribute.isGeneralAttribute()) {
          generalBooleanValues.put(booleanAttribute, booleanValue);
        } else {
          log.warn(GT._("Attribute {0} can''t be set in general configuration", name));
        }
      } else {
        if (booleanAttribute.isUserAttribute()) {
          userBooleanValues.put(booleanAttribute, booleanValue);
        } else {
          log.warn(GT._("Attribute {0} can''t be set in user configuration", name));
        }
      }
      return;
    }

    // Check if it is a String attribute
    WPCConfigurationString stringAttribute = WPCConfigurationString.getValue(name);
    if (stringAttribute != null) {
      if (general) {
        if (stringAttribute.isGeneralAttribute()) {
          generalStringValues.put(stringAttribute, value);
        } else {
          log.warn(GT._("Attribute {0} can''t be set in general configuration", name));
        }
      } else {
        if (stringAttribute.isUserAttribute()) {
          userStringValues.put(stringAttribute, value);
        } else {
          log.warn(GT._("Attribute {0} can''t be set in user configuration", name));
        }
      }
      return;
    }

    // Check if it is a String list attribute
    WPCConfigurationStringList listAttribute = WPCConfigurationStringList.getValue(name);
    if (listAttribute != null) {
      List<String> listValue = convertPropertyToStringList(value);
      if (general) {
        if (listAttribute.isGeneralAttribute()) {
          generalStringListValues.put(listAttribute, listValue);
        } else {
          log.warn(GT._("Attribute {0} can''t be set in general configuration", name));
        }
      } else {
        if (listAttribute.isUserAttribute()) {
          userStringListValues.put(listAttribute, listValue);
        } else {
          log.warn(GT._("Attribute {0} can''t be set in user configuration", name));
        }
      }
      return;
    }

    // Check if it is a Long attribute
    WPCConfigurationLong longAttribute = WPCConfigurationLong.getValue(name);
    if (longAttribute != null) {
      try {
        Long longValue = Long.valueOf(value);
        if (general) {
          if (longAttribute.isGeneralAttribute()) {
            generalLongValues.put(longAttribute, longValue);
          } else {
            log.warn(GT._("Attribute {0} can''t be set in general configuration", name));
          }
        } else {
          if (longAttribute.isUserAttribute()) {
            userLongValues.put(longAttribute, longValue);
          } else {
            log.warn(GT._("Attribute {0} can''t be set in user configuration", name));
          }
        }
      } catch (NumberFormatException e) {
        log.warn(GT._("Attribute {0} should be integer value", name));
      }
      return;
    }

    // Properties available also in user configuration
    if (name.startsWith("error_")) {
      wiki.getCWConfiguration().setUserConfiguration(name, value);
      return;
    } else if (!general) {
      log.warn(GT._("Attribute {0} can''t be set in user configuration", name));
      return;
    }

    // Properties available only in general configuration
    if (name.equals("general_encyclopedic_namespaces")) {
      setEncyclopedicNamespaces(value);
    } else if (name.equals("general_dab_1l_templates")) {
      setTemplateMatchersDab1L(value);
    } else if (name.equals("general_dab_1lt_templates")) {
      setTemplateMatchersDab1LT(value);
    } else if (name.equals("general_dab_1l2t_templates")) {
      setTemplateMatchersDab1L2T(value);
    } else if (name.equals("general_good_1l_templates")) {
      setTemplateMatchersGood1L(value);
    } else if (name.equals("general_good_1lt_templates")) {
      setTemplateMatchersGood1LT(value);
    } else if (name.equals("general_help_1l_templates")) {
      setTemplateMatchersHelp1L(value);
    } else if (name.equals("general_help_1lt_templates")) {
      setTemplateMatchersHelp1LT(value);
    } else if (name.equals("dab_categories")) {
      setDisambiguationCategories(value);
    } else if (name.equals("dab_ask_help_templates_after")) {
      setTemplatesAfterAskHelp(value);
    } else if (name.equals("wikt_templates")) {
      setWiktionaryMatches(value);
    } else if (name.equals("check_wiki_project_page")) {
      wiki.getCWConfiguration().setProjectPage(value);
    } else if (name.equals("check_wiki_comment")) {
      wiki.getCWConfiguration().setComment(value);
    } else if (name.equals("check_wiki_translation_page")) {
      wiki.getCWConfiguration().setTranslationPage(value);
    } else if (name.equals("check_wiki_force")) {
      wiki.getCWConfiguration().setForce(value);
    } else {
      log.warn(GT._("Attribute {0} can''t be set in general configuration", name));
    }
  }

  /* ================================================================================= */
  /* General                                                                           */
  /* ================================================================================= */

  /**
   * Encyclopedic name spaces.
   */
  private List<Integer> encyclopedicNamespaces;

  /**
   * Encyclopedic talk name spaces.
   */
  private List<Integer> encyclopedicTalkNamespaces;

  /**
   * @param value Encyclopedic name spaces.
   */
  private void setEncyclopedicNamespaces(String value) {
    encyclopedicNamespaces.clear();
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      if (tmpList != null) {
        for (String tmp : tmpList) {
          try {
            Integer intValue = Integer.valueOf(tmp);
            if (intValue.intValue() % 2 == 0) {
              encyclopedicNamespaces.add(intValue);
            }
          } catch (NumberFormatException e) {
            //
          }
        }
        initEncyclopedicTalkNamespaces();
      }
    } else {
      initDefaultEncyclopedicNamespaces();
    }
  }

  /**
   * Default initialization of encyclopedic name spaces.
   */
  private void initDefaultEncyclopedicNamespaces() {
    encyclopedicNamespaces = new ArrayList<Integer>();
    encyclopedicNamespaces.add(Namespace.MAIN);
    encyclopedicNamespaces.add(Namespace.IMAGE);
    encyclopedicNamespaces.add(Namespace.TEMPLATE);
    encyclopedicNamespaces.add(Namespace.CATEGORY);
    initEncyclopedicTalkNamespaces();
  }

  /**
   * Initialization of encyclopedic talk name spaces.
   */
  private void initEncyclopedicTalkNamespaces() {
    if (encyclopedicNamespaces == null) {
      encyclopedicTalkNamespaces = null;
      return;
    }
    encyclopedicTalkNamespaces = new ArrayList<Integer>(encyclopedicNamespaces.size());
    for (Integer namespace : encyclopedicNamespaces) {
      encyclopedicTalkNamespaces.add(Integer.valueOf(namespace.intValue() + 1));
    }
  }

  /**
   * @param namespace Name space.
   * @return True if the name space is encyclopedic.
   */
  public boolean isEncyclopedicNamespace(Integer namespace) {
    if ((encyclopedicNamespaces == null) || (namespace == null)) {
      return false;
    }
    return encyclopedicNamespaces.contains(namespace);
  }

  /**
   * @return Encyclopedic name spaces.
   */
  public List<Integer> getEncyclopedicNamespaces() {
    return encyclopedicNamespaces;
  }

  /**
   * @return Encyclopedic talk name spaces.
   */
  public List<Integer> getEncyclopedicTalkNamespaces() {
    return encyclopedicTalkNamespaces;
  }

  /* ================================================================================= */
  /* Suggestions                                                                       */
  /* ================================================================================= */

  /**
   * Spelling suggestions.
   */
  private Map<String, Suggestion> suggestions;

  /**
   * Initialize suggestions for text replacements.
   * 
   * @param api
   */
  public void initSuggestions(API api, boolean forceInit) {
    if ((suggestions == null) || forceInit) {
      synchronized (api) {

        // Load all pages contents
        Map<String, Page> pages = new HashMap<String, Page>();
        List<String> suggestionPages = getStringList(WPCConfigurationStringList.SUGGESTION_PAGES);
        if (suggestionPages != null) {
          for (String suggestionPage : suggestionPages) {
            String[] elements = suggestionPage.split("\\|");
            if (elements.length >= 4) {
              String pageName = elements[0];
              if (!pages.containsKey(pageName)) {
                pages.put(pageName, DataManager.getPage(wiki, pageName, null, null));
              }
            }
          }
        }
        List<String> suggestionTypoPages = getStringList(WPCConfigurationStringList.SUGGESTION_TYPO_PAGES);
        if (suggestionTypoPages != null) {
          for (String suggestionPage : suggestionTypoPages) {
            if (!pages.containsKey(suggestionPage)) {
              pages.put(suggestionPage, DataManager.getPage(wiki, suggestionPage, null, null));
            }
          }
        }
        try {
          api.retrieveContents(wiki, pages.values(), false);
        } catch (APIException e) {
          System.err.println("Exception retrieving contents for suggestions");
        }

        // Construct suggestions
        Map<String, Suggestion> tmpMap = new HashMap<String, Suggestion>();
        if (suggestionPages != null) {
          List<String> suggestionIgnore = getStringList(WPCConfigurationStringList.SUGGESTION_IGNORE);
          for (String suggestionPage : suggestionPages) {
            String[] elements = suggestionPage.split("\\|");
            if (elements.length >= 4) {
              String pageName = elements[0];
              String[] elementsReplacement = elements[3].split(",");
              Page page = pages.get(pageName);
              if ((page != null) && (page.getContents() != null)) {
                String contents = page.getContents();
                PageAnalysis analysis = page.getAnalysis(contents, true);
                Collection<PageElementTemplate> templates = analysis.getTemplates(elements[1]);
                for (PageElementTemplate template : templates) {
                  String chapterId = PageAnalysisUtils.getCurrentChapterId(analysis, template.getBeginIndex());
                  if ((suggestionIgnore == null) || (!suggestionIgnore.contains(chapterId))) {
                    String patternText = template.getParameterValue(elements[2]);
                    Suggestion suggestion = tmpMap.get(patternText);
                    if (suggestion == null) {
                      String chapter = PageAnalysisUtils.getCurrentChapterId(analysis, template.getBeginIndex());
                      suggestion = Suggestion.createSuggestion(patternText, false, chapter);
                      if (suggestion != null) {
                        tmpMap.put(patternText, suggestion);
                      }
                    }
                    if (suggestion != null) {
                      if (elements.length > 4) {
                        suggestion.setComment(template.getParameterValue(elements[4]));
                      }
                      for (String elementReplacement : elementsReplacement) {
                        String replacementText = template.getParameterValue(elementReplacement);
                        if ((replacementText != null) &&
                            (replacementText.length() > 0)) {
                          suggestion.addReplacement(replacementText);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }

        // Construct suggestions from AWB format
        if (suggestionTypoPages != null) {
          List<String> suggestionIgnore = getStringList(WPCConfigurationStringList.SUGGESTION_IGNORE);
          for (String suggestionPage : suggestionTypoPages) {
            Page page = pages.get(suggestionPage);
            if ((page != null) && (page.getContents() != null)) {
              String contents = page.getContents();
              PageAnalysis analysis = page.getAnalysis(contents, true);
              Collection<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_OTHER_TYPO);
              for (PageElementTag tag : tags) {
                String chapterId = PageAnalysisUtils.getCurrentChapterId(analysis, tag.getBeginIndex());
                if ((suggestionIgnore == null) || (!suggestionIgnore.contains(chapterId))) {
                  Parameter word = tag.getParameter("word");
                  Parameter find = tag.getParameter("find");
                  Parameter replace = tag.getParameter("replace");
                  Parameter disabled = tag.getParameter("disabled");
                  if ((word != null) && (find != null) && (replace != null) && (disabled == null)) {
                    String wordValue = word.getValue();
                    String findValue = find.getValue();
                    String replaceValue = replace.getValue();
                    if ((wordValue != null) && (findValue != null) && (replaceValue != null)) {
                      String cleanFindValue = Suggestion.cleanPattern(findValue);
                      if (cleanFindValue == null) {
                        System.err.println("Rejecting " + wordValue + " : " + findValue);
                      } else {
                        Suggestion suggestion = tmpMap.get(cleanFindValue);
                        if (suggestion == null) {
                          String chapter = PageAnalysisUtils.getCurrentChapterId(analysis, tag.getBeginIndex());
                          suggestion = Suggestion.createSuggestion(cleanFindValue, true, chapter);
                          if (suggestion != null) {
                            tmpMap.put(cleanFindValue, suggestion);
                          }
                        }
                        if (suggestion != null) {
                          suggestion.setComment("Typo AWB " + wordValue);
                          suggestion.addReplacement(replaceValue);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }

        suggestions = tmpMap;
      }
    }
  }

  /**
   * @return Suggestions.
   */
  public Map<String, Suggestion> getSuggestions() {
    return suggestions;
  }

  /* ================================================================================= */
  /* Template matches                                                                  */
  /* ================================================================================= */

  private Map<String, List<TemplateMatcher>> templateMatchers;

  /**
   * @return Some matchers exist ?
   */
  public boolean hasTemplateMatchers() {
    if (templateMatchers == null) {
      return false;
    }
    if (templateMatchers.isEmpty()) {
      return false;
    }
    return true;
  }

  private void setTemplateMatchersDab1L(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterList = (elements.length > 1) ? elements[1] : null;
        String explanation = (elements.length > 2) ? elements[2] : null;
        String defaultValue = (elements.length > 3) ? elements[3] : null;
        String neededParameter = (elements.length > 4) ? elements[4] : null;
        if ((templateName != null) && (parameterList != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          String[] parameterNames = parameterList.split(",");
          for (String parameterName : parameterNames) {
            TemplateMatcher matcher = new TemplateMatcher1L(
                wiki, templateName, explanation, false, false,
                parameterName, defaultValue, neededParameter);
            list.add(matcher);
          }
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  private void setTemplateMatchersDab1LT(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterList = (elements.length > 1) ? elements[1] : null;
        String explanation = (elements.length > 2) ? elements[2] : null;
        String defaultValue = (elements.length > 3) ? elements[3] : null;
        String neededParameter = (elements.length > 4) ? elements[4] : null;
        if ((templateName != null) && (parameterList != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          String[] parameterNames = parameterList.split(",");
          for (String parameterName : parameterNames) {
            TemplateMatcher matcher = new TemplateMatcher1LT(
                wiki, templateName, explanation, false, false,
                parameterName, defaultValue, neededParameter);
            list.add(matcher);
          }
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  private void setTemplateMatchersDab1L2T(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterName1 = (elements.length > 1) ? elements[1] : null;
        String parameterName2 = (elements.length > 2) ? elements[2] : null;
        String explanation = (elements.length > 3) ? elements[3] : null;
        if ((templateName != null) && (parameterName1 != null) && (parameterName2 != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          TemplateMatcher matcher = new TemplateMatcher1L2T(
              wiki, templateName, explanation,
              parameterName1, parameterName2);
          list.add(matcher);
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  private void setTemplateMatchersGood1L(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterList = (elements.length > 1) ? elements[1] : null;
        String explanation = (elements.length > 2) ? elements[2] : null;
        String defaultValue = (elements.length > 3) ? elements[3] : null;
        String neededParameter = (elements.length > 4) ? elements[4] : null;
        if ((templateName != null) && (parameterList != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          String[] parameterNames = parameterList.split(",");
          for (String parameterName : parameterNames) {
            TemplateMatcher matcher = new TemplateMatcher1L(
                wiki, templateName, explanation, true, false,
                parameterName, defaultValue, neededParameter);
            list.add(matcher);
          }
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  private void setTemplateMatchersGood1LT(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterList = (elements.length > 1) ? elements[1] : null;
        String explanation = (elements.length > 2) ? elements[2] : null;
        String defaultValue = (elements.length > 3) ? elements[3] : null;
        String neededParameter = (elements.length > 4) ? elements[4] : null;
        if ((templateName != null) && (parameterList != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          String[] parameterNames = parameterList.split(",");
          for (String parameterName : parameterNames) {
            TemplateMatcher matcher = new TemplateMatcher1LT(
                wiki, templateName, explanation, true, false,
                parameterName, defaultValue, neededParameter);
            list.add(matcher);
          }
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  private void setTemplateMatchersHelp1L(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterList = (elements.length > 1) ? elements[1] : null;
        String explanation = (elements.length > 2) ? elements[2] : null;
        String defaultValue = (elements.length > 3) ? elements[3] : null;
        String neededParameter = (elements.length > 4) ? elements[4] : null;
        if ((templateName != null) && (parameterList != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          String[] parameterNames = parameterList.split(",");
          for (String parameterName : parameterNames) {
            TemplateMatcher matcher = new TemplateMatcher1L(
                wiki, templateName, explanation, false, true,
                parameterName, defaultValue, neededParameter);
            list.add(matcher);
          }
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  private void setTemplateMatchersHelp1LT(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      List<String> tmpList = convertPropertyToStringList(value);
      for (String template : tmpList) {
        String[] elements = template.split("\\|");
        String templateName = (elements.length > 0) ? wiki.normalizeTitle(elements[0]) : null;
        String parameterList = (elements.length > 1) ? elements[1] : null;
        String explanation = (elements.length > 2) ? elements[2] : null;
        String defaultValue = (elements.length > 3) ? elements[3] : null;
        String neededParameter = (elements.length > 4) ? elements[4] : null;
        if ((templateName != null) && (parameterList != null)) {
          List<TemplateMatcher> list = templateMatchers.get(templateName);
          if (list == null) {
            list = new ArrayList<TemplateMatcher>();
          }
          String[] parameterNames = parameterList.split(",");
          for (String parameterName : parameterNames) {
            TemplateMatcher matcher = new TemplateMatcher1LT(
                wiki, templateName, explanation, false, true,
                parameterName, defaultValue, neededParameter);
            list.add(matcher);
          }
          templateMatchers.put(templateName, list);
        }
      }
    }
  }

  /**
   * @return Matchers for templates creating direct internal links from parameter value.
   */
  public List<TemplateMatcher> getTemplateMatchers(String templateName) {
    if (templateMatchers != null) {
      return templateMatchers.get(wiki.normalizeTitle(templateName));
    }
    return null;
  }

  /* ================================================================================= */
  /* Disambiguation                                                                    */
  /* ================================================================================= */

  /**
   * Categories allowing to decide if a page is a disambiguation page.
   */
  private List<Page> disambiguationCategories;

  /**
   * @param value Categories allowing to decide if a page is a disambiguation page.
   */
  private void setDisambiguationCategories(String value) {
    List<String> tmp = convertPropertyToStringList(value);
    if ((tmp != null) && (tmp.size() > 0)) {
      this.disambiguationCategories = new ArrayList<Page>(tmp.size());
      for (String category : tmp) {
        this.disambiguationCategories.add(
            DataManager.getPage(wiki, category, null, null));
      }
    } else {
      this.disambiguationCategories = null;
    }
  }

  /**
   * @return Categories allowing to decide if a page is a disambiguation page.
   */
  public List<Page> getDisambiguationCategories() {
    return disambiguationCategories;
  }

  /**
   * @param count Number of disambiguation links that have been fixed.
   * @param links Links that have been fixed.
   * @return Comment for disambiguation links that have been fixed.
   */
  public String getDisambiguationComment(int count, List<String> links) {
    String comment = null;
    if (count == 1) {
      comment = getString(WPCConfigurationString.DAB_COMMENT_1);
    }
    if (comment == null) {
      comment = getString(WPCConfigurationString.DAB_COMMENT);
    }
    if (comment == null) {
      return "";
    }
    try {
      comment = MessageFormat.format(comment, Integer.valueOf(count));
      if (links != null) {
        StringBuilder sb = new StringBuilder();
        for (String link : links) {
          sb.append(sb.length() > 0 ? ", " : " - ");
          sb.append("[[" + link + "]]");
        }
        comment += sb.toString();
      }
    } catch (IllegalArgumentException e) {
      //
    }
    return comment;
  }

  /**
   * @param count Number of disambiguation links for which help has been requested.
   * @param links Links for which help has been requested.
   * @return Comment for disambiguation links for which help has been requested.
   */
  public String getDisambiguationCommentHelp(int count, List<String> links) {
    String comment = null;
    if (count == 1) {
      comment = getString(WPCConfigurationString.DAB_COMMENT_HELP_1);
    }
    if (comment == null) {
      comment = getString(WPCConfigurationString.DAB_COMMENT_HELP);
    }
    if (comment == null) {
      return "";
    }
    try {
      comment = MessageFormat.format(comment, Integer.valueOf(count));
      if (links != null) {
        StringBuilder sb = new StringBuilder();
        for (String link : links) {
          sb.append(sb.length() > 0 ? ", " : " - ");
          sb.append("[[" + link + "]]");
        }
        comment += sb.toString();
      }
    } catch (IllegalArgumentException e) {
      //
    }
    return comment;
  }

  /**
   * @param count Number of disambiguation links that still need to be fixed.
   * @param links Links that still need to be fixed.
   * @return Comment for disambiguation links that still need to be fixed.
   */
  public String getDisambiguationCommentTodo(int count, List<String> links) {
    String comment = null;
    if (count == 1) {
      comment = getString(WPCConfigurationString.DAB_COMMENT_TODO_1);
    }
    if (comment == null) {
      comment = getString(WPCConfigurationString.DAB_COMMENT_TODO);
    }
    if (comment == null) {
      return "";
    }
    try {
      comment = MessageFormat.format(comment, Integer.valueOf(count));
      if (links != null) {
        StringBuilder sb = new StringBuilder();
        for (String link : links) {
          sb.append(sb.length() > 0 ? ", " : " - ");
          sb.append("[[" + link + "]]");
        }
        comment += sb.toString();
      }
    } catch (IllegalArgumentException e) {
      //
    }
    return comment;
  }

  /**
   * @return Comment for updating a page.
   */
  public String getUpdatePageMessage() {
    return ""; // TODO
  }

  /* ================================================================================= */
  /* Disambiguation warning                                                            */
  /* ================================================================================= */

  /**
   * @param count Number of disambiguation links.
   * @return Comment for warning about disambiguation links in a page.
   */
  public String getDisambiguationWarningComment(int count) {
    if (count == 1) {
      String comment1 = getString(WPCConfigurationString.DAB_WARNING_COMMENT_1);
      if ((comment1 != null) && (comment1.length() > 0)) {
        return comment1;
      }
    }
    String comment = getString(WPCConfigurationString.DAB_WARNING_COMMENT);
    if (comment != null) {
      try {
        return MessageFormat.format(comment, Integer.valueOf(count));
      } catch (IllegalArgumentException e) {
        //
      }
      return comment;
    }
    return getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
  }

  /**
   * @return Comment for telling that disambiguation links have been fixed.
   */
  public String getDisambiguationWarningCommentDone() {
    String comment = getString(WPCConfigurationString.DAB_WARNING_COMMENT_DONE);
    if (comment != null) {
      return comment;
    }
    return getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
  }

  /* ================================================================================= */
  /* Templates                                                                         */
  /* ================================================================================= */

  /**
   * Templates to be used after a disambiguation link to ask for help.
   */
  private List<List<String>> templatesAfterAskHelp;

  /**
   * @param value Templates to be used after a disambiguation link to ask for help.
   */
  private void setTemplatesAfterAskHelp(String value) {
    List<String> tmp = convertPropertyToStringList(value);
    if (tmp != null) {
      List<List<String>> result = new ArrayList<List<String>>(tmp.size());
      for (String element : tmp) {
        int pipeIndex = element.indexOf("|");
        if (pipeIndex < 0) {
          result.add(Collections.singletonList(element));
        } else {
          List<String> tmpElement = new ArrayList<String>(2);
          tmpElement.add(element.substring(0, pipeIndex));
          tmpElement.add(element.substring(pipeIndex + 1));
          result.add(tmpElement);
        }
      }
      this.templatesAfterAskHelp = result;
    } else {
      this.templatesAfterAskHelp = null;
    }
  }

  /**
   * @return Templates to be used after a disambiguation link to ask for help.
   */
  public List<List<String>> getTemplatesAfterAskHelp() {
    return templatesAfterAskHelp;
  }

  /**
   * @return Templates used for finding pages where help is requested.
   */
  public List<Page> getTemplatesForHelpRequested() {
    List<String> templatesForHelpRequested = getStringList(
        WPCConfigurationStringList.TEMPLATES_FOR_HELP_REQUESTED);
    if (templatesForHelpRequested != null) {
      List<Page> tmp = new ArrayList<Page>(templatesForHelpRequested.size());
      for (String template : templatesForHelpRequested) {
        String title = wiki.getWikiConfiguration().getPageTitle(
            Namespace.TEMPLATE, template);
        tmp.add(DataManager.getPage(wiki, title, null, null));
      }
      return tmp;
    }
    return null;
  }

  /* ================================================================================= */
  /* Wiktionary                                                                        */
  /* ================================================================================= */

  private List<TemplateMatch> wiktionaryMatches;

  private void setWiktionaryMatches(String value) {
    List<String> tmpList = convertPropertyToStringList(value);
    if ((tmpList != null) && (tmpList.size() > 0)) {
      wiktionaryMatches = new ArrayList<TemplateMatch>(tmpList.size());
      for (String tmp : tmpList) {
        String[] elements = tmp.split("\\|");
        TemplateMatch match = new TemplateMatch(
            (elements.length > 0) ? elements[0].trim() : "",
            (elements.length > 1) ? elements[1].trim() : "",
            (elements.length > 2) ? elements[2].trim() : "",
            true, false);
        wiktionaryMatches.add(match);
      }
    }
  }

  /**
   * @param page Page name.
   * @return Flag indicating if <code>page</code> is a wiktionary template.
   */
  public boolean isWiktionaryTemplate(String page) {
    if ((wiktionaryMatches == null) || (page == null)) {
      return false;
    }
    for (TemplateMatch element : wiktionaryMatches) {
      if (page.equals(wiki.getWikiConfiguration().getPageTitle(
          Namespace.TEMPLATE,
          element.getName()))) {
        return true;
      }
    }
    return false;
  }

  /**
   * @return Count of wiktionary templates.
   */
  public int getWiktionaryMatchesCount() {
    if (wiktionaryMatches != null) {
      return wiktionaryMatches.size();
    }
    return 0;
  }

  /**
   * @param index Wiktionary template index.
   * @return Wiktionary template to analyze for links.
   */
  public TemplateMatch getWiktionaryMatch(int index) {
    if (wiktionaryMatches != null) {
      return wiktionaryMatches.get(index);
    }
    return null;
  }

  /* ================================================================================= */
  /* Utilities                                                                         */
  /* ================================================================================= */

  /**
   * Convert a multi-line property to a string list.
   * 
   * @param property Property.
   * @return String list.
   */
  public static List<String> convertPropertyToStringList(String property) {
    List<String> result = null;
    if ((property != null) && (property.trim().length() > 0)) {
      String[] results = property.trim().split("\n");
      if ((results != null) && (results.length > 0)) {
        result = new ArrayList<String>();
        for (int  i = 0; i < results.length; i++) {
          results[i] = results[i].trim();
          if (results[i].length() > 0) {
            result.add(results[i]);
          }
        }
      }
    }
    return result;
  }
}
