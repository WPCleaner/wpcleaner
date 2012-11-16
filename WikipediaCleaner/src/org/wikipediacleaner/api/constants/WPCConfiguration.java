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
  private final Log log = LogFactory.getLog(WPCConfiguration.class);

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
    generalBooleanValues = new HashMap<WPCConfigurationAttributeBoolean, Boolean>();
    userBooleanValues = new HashMap<WPCConfigurationAttributeBoolean, Boolean>();
    generalStringValues = new HashMap<WPCConfigurationAttributeString, String>();
    userStringValues = new HashMap<WPCConfigurationAttributeString, String>();
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
    initDefaultEncyclopedicNamespaces();
    currentDisambiguationList = null;
    disambiguationCategories = null;
    disambiguationWarningAfterTemplates = null;
    mostDisambiguationLinks = null;
    suggestionIgnore = null;
    suggestionPages = null;
    suggestionTypoPages = null;
    suggestions = null;
    templateMatchers = new HashMap<String, List<TemplateMatcher>>();
    templatesAfterAskHelp = null;
    templatesAfterHelpAsked = null;
    templatesForDisambiguationLink = null;
    templatesForHelpRequested = null;
    templatesForLinkingText = null;
    templatesForNeedingHelp = null;
    todoLinkTemplates = null;
    todoTemplates = null;
    wiktionaryInterwiki = null;
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
  private final Map<WPCConfigurationAttributeBoolean, Boolean> generalBooleanValues;

  /**
   * Map for holding boolean values for user settings.
   */
  private final Map<WPCConfigurationAttributeBoolean, Boolean> userBooleanValues;

  /**
   * Map for holding String values for general settings.
   */
  private final Map<WPCConfigurationAttributeString, String> generalStringValues;

  /**
   * Map for holding String values for user settings.
   */
  private final Map<WPCConfigurationAttributeString, String> userStringValues;

  /**
   * @param attribute Attribute.
   * @return Attribute value.
   */
  public boolean getBooleanProperty(WPCConfigurationAttributeBoolean attribute) {
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
   * @param attribute Attribute.
   * @return Attribute value.
   */
  public String getStringProperty(WPCConfigurationAttributeString attribute) {
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
    WPCConfigurationAttributeBoolean booleanAttribute = WPCConfigurationAttributeBoolean.getValue(name);
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
    WPCConfigurationAttributeString stringAttribute = WPCConfigurationAttributeString.getValue(name);
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
    }

    // Properties available also in user configuration
    if (name.equals("general_suggestions")) {
      setSuggestionPages(value, general);
    } else if (name.equals("general_suggestions_typo")) {
      setSuggestionTypoPages(value, general);
    } else if (name.equals("general_suggestions_ignore")) {
      setSuggestionIgnore(value, general);
    } else if (name.startsWith("error_")) {
      wiki.getCWConfiguration().setUserConfiguration(name, value);
    } else if (!general) {
      return;
    }

    // Properties available only in general configuration
    if (name.equals("general_encyclopedic_namespaces")) {
      setEncyclopedicNamespaces(value);
    } else if (name.equals("general_todo_templates")) {
      setTodoTemplates(value);
    } else if (name.equals("general_todo_link_templates")) {
      setTodoLinkTemplates(value);
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
    } else if (name.equals("dab_list")) {
      setCurrentDisambiguationList(value);
    } else if (name.equals("most_dab_links")) {
      setMostDisambiguationLinks(value);
    } else if (name.equals("dab_ask_help_templates_after")) {
      setTemplatesAfterAskHelp(value);
    } else if (name.equals("dab_help_asked_templates_after")) {
      setTemplatesAfterHelpAsked(value);
    } else if (name.equals("dab_warning_after_templates")) {
      setDisambiguationWarningAfterTemplates(value);
    } else if (name.equals("dab_link_templates")) {
      setTemplatesForDisambiguationLink(value);
    } else if (name.equals("needing_help_templates")) {
      setTemplatesForNeedingHelp(value);
    } else if (name.equals("help_requested_templates")) {
      setTemplatesForHelpRequested(value);
    } else if (name.equals("link_text_templates")) {
      setTemplatesForLinkingText(value);
    } else if (name.equals("wikt_interwiki")) {
      setWiktionaryInterwiki(value);
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
   * @return Encyclopedic talk name spaces.
   */
  public List<Integer> getEncyclopedicTalkNamespaces() {
    return encyclopedicTalkNamespaces;
  }

  /* ================================================================================= */
  /* To do lists                                                                       */
  /* ================================================================================= */

  /**
   * Templates creating "to do" lists.
   */
  private List<String> todoTemplates;

  /**
   * Templates creating links to "to do" lists.
   */
  private List<String> todoLinkTemplates;

  /**
   * @param value Templates creating "to do" lists.
   */
  private void setTodoTemplates(String value) {
    todoTemplates = convertPropertyToStringList(value);
  }

  /**
   * @param value Templates creating links to "to do" lists.
   */
  private void setTodoLinkTemplates(String value) {
    this.todoLinkTemplates = convertPropertyToStringList(value);
  }

  /**
   * @return Templates creating "to do" lists.
   */
  public List<String> getTodoTemplates() {
    return todoTemplates;
  }

  /**
   * @return Templates creating links to "to do" lists.
   */
  public List<String> getTodoLinkTemplates() {
    return todoLinkTemplates;
  }

  /* ================================================================================= */
  /* Suggestions                                                                       */
  /* ================================================================================= */

  /**
   * Pages containing spelling suggestions.
   */
  private List<String> suggestionPages;

  /**
   * Pages containing spelling suggestions in AWB format.
   */
  private List<String> suggestionTypoPages;

  /**
   * Chapters to be ignored for suggestions.
   */
  private List<String> suggestionIgnore;

  /**
   * Spelling suggestions.
   */
  private Map<String, Suggestion> suggestions;

  /**
   * @param value Pages containing spelling suggestions.
   * @param general Flag indicating if dealing with general or user properties.
   */
  private void setSuggestionPages(String value, boolean general) {
    if (general || (suggestionPages == null)) {
      suggestionPages = convertPropertyToStringList(value);
    } else {
      suggestionPages.addAll(convertPropertyToStringList(value));
    }
  }

  /**
   * @param value Pages containing spelling suggestions in AWB format.
   * @param general Flag indicating if dealing with general or user properties.
   */
  private void setSuggestionTypoPages(String value, boolean general) {
    if (general || (suggestionTypoPages == null)) {
      suggestionTypoPages = convertPropertyToStringList(value);
    } else {
      suggestionTypoPages.addAll(convertPropertyToStringList(value));
    }
  }

  /**
   * @param value Chapters to be ignored for suggestions.
   * @param general Flag indicating if dealing with general or user properties.
   */
  private void setSuggestionIgnore(String value, boolean general) {
    if (general || (suggestionIgnore == null)) {
      suggestionIgnore = convertPropertyToStringList(value);
    } else {
      suggestionIgnore.addAll(convertPropertyToStringList(value));
    }
  }

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
   * Pages containing the current list of disambiguation pages.
   */
  private List<String> currentDisambiguationList;

  /**
   * Pages containing the list of pages with many disambiguation links.
   */
  private List<String> mostDisambiguationLinks;

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
   * @param value Pages containing the current list of disambiguation pages.
   */
  private void setCurrentDisambiguationList(String value) {
    this.currentDisambiguationList = convertPropertyToStringList(value);
  }

  /**
   * @param value Pages containing the list of pages with many disambiguation links.
   */
  private void setMostDisambiguationLinks(String value) {
    this.mostDisambiguationLinks = convertPropertyToStringList(value);
  }

  /**
   * @return Categories allowing to decide if a page is a disambiguation page.
   */
  public List<Page> getDisambiguationCategories() {
    return disambiguationCategories;
  }

  /**
   * @return Pages containing the current list of disambiguation pages.
   */
  public List<String> getCurrentDisambiguationList() {
    return currentDisambiguationList;
  }

  /**
   * @return Pages containing the list of pages with many disambiguation links.
   */
  public List<String> getMostDisambiguationLinks() {
    return mostDisambiguationLinks;
  }

  /**
   * @param count Number of disambiguation links that have been fixed.
   * @return Comment for disambiguation links that have been fixed.
   */
  public String getDisambiguationComment(int count) {
    if (count == 1) {
      String comment1 = getStringProperty(WPCConfigurationAttributeString.DAB_COMMENT_1);
      if (comment1 != null) {
        return comment1;
      }
    }
    String comment = getStringProperty(WPCConfigurationAttributeString.DAB_COMMENT);
    if (comment != null) {
      try {
        return MessageFormat.format(comment, Integer.valueOf(count));
      } catch (IllegalArgumentException e) {
        //
      }
      return comment;
    }
    return "";
  }

  /**
   * @param count Number of disambiguation links that still need to be fixed.
   * @return Comment for disambiguation links that still need to be fixed.
   */
  public String getDisambiguationCommentTodo(int count) {
    if (count == 1) {
      String comment1 = getStringProperty(WPCConfigurationAttributeString.DAB_COMMENT_TODO_1);
      if (comment1 != null) {
        return comment1;
      }
    }
    String comment = getStringProperty(WPCConfigurationAttributeString.DAB_COMMENT_TODO);
    try {
      return MessageFormat.format(comment, Integer.valueOf(count));
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
   * List of templates that should be before the disambiguation warning.
   */
  private List<String> disambiguationWarningAfterTemplates;

  /**
   * @param value List of templates that should be before the disambiguation warning.
   */
  private void setDisambiguationWarningAfterTemplates(String value) {
    this.disambiguationWarningAfterTemplates = convertPropertyToStringList(value);
  }

  /**
   * @return List of templates that should be before the disambiguation warning.
   */
  public List<String> getDisambiguationWarningAfterTemplates() {
    return disambiguationWarningAfterTemplates;
  }

  /**
   * @param count Number of disambiguation links.
   * @return Comment for warning about disambiguation links in a page.
   */
  public String getDisambiguationWarningComment(int count) {
    if (count == 1) {
      String comment1 = getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_COMMENT_1);
      if ((comment1 != null) && (comment1.length() > 0)) {
        return comment1;
      }
    }
    String comment = getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_COMMENT);
    if (comment != null) {
      try {
        return MessageFormat.format(comment, Integer.valueOf(count));
      } catch (IllegalArgumentException e) {
        //
      }
      return comment;
    }
    return getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE);
  }

  /**
   * @return Comment for telling that disambiguation links have been fixed.
   */
  public String getDisambiguationWarningCommentDone() {
    String comment = getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_COMMENT_DONE);
    if (comment != null) {
      return comment;
    }
    return getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE);
  }

  /* ================================================================================= */
  /* Templates                                                                         */
  /* ================================================================================= */

  /**
   * Templates used for a normal link to a disambiguation page.
   */
  private List<String> templatesForDisambiguationLink;

  /**
   * Templates to be used after a disambiguation link to ask for help.
   */
  private List<List<String>> templatesAfterAskHelp;

  /**
   * Templates used after a disambiguation link asking for help.
   */
  private List<String> templatesAfterHelpAsked;

  /**
   * Templates used for a link where help is required.
   */
  private List<String> templatesForNeedingHelp;

  /**
   * Templates used for finding pages where help is requested.
   */
  private List<String> templatesForHelpRequested;

  /**
   * Templates used for linking text.
   */
  private List<String> templatesForLinkingText;

  /**
   * @param value Templates used for a normal link to a disambiguation page.
   */
  private void setTemplatesForDisambiguationLink(String value) {
    this.templatesForDisambiguationLink = convertPropertyToStringList(value);
  }

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
   * @param value Templates used after a disambiguation link asking for help.
   */
  private void setTemplatesAfterHelpAsked(String value) {
    this.templatesAfterHelpAsked = convertPropertyToStringList(value);
  }

  /**
   * @param value Templates used for a link where help is required.
   */
  private void setTemplatesForNeedingHelp(String value) {
    this.templatesForNeedingHelp = convertPropertyToStringList(value);
  }

  /**
   * @param value Templates used for finding pages where help is requested.
   */
  private void setTemplatesForHelpRequested(String value) {
    templatesForHelpRequested = convertPropertyToStringList(value);
  }

  /**
   * @param value Templates used for linking text.
   */
  private void setTemplatesForLinkingText(String value) {
    templatesForLinkingText = convertPropertyToStringList(value);
  }

  /**
   * @return Templates used for a normal link to a disambiguation page.s
   */
  public List<String> getTemplatesForDisambiguationLink() {
    if (templatesForDisambiguationLink != null) {
      return new ArrayList<String>(templatesForDisambiguationLink);
    }
    return null;
  }

  /**
   * @return Templates to be used after a disambiguation link to ask for help.
   */
  public List<List<String>> getTemplatesAfterAskHelp() {
    return templatesAfterAskHelp;
  }

  /**
   * @return Templates used after a disambiguation link asking for help.
   */
  public List<String> getTemplatesAfterHelpAsked() {
    return templatesAfterHelpAsked;
  }

  /**
   * @return Templates used for a link where help is required.
   */
  public List<String> getTemplatesForNeedingHelp() {
    if (templatesForNeedingHelp != null) {
      return new ArrayList<String>(templatesForNeedingHelp);
    }
    return null;
  }

  /**
   * @return Templates used for finding pages where help is requested.
   */
  public List<Page> getTemplatesForHelpRequested() {
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

  /**
   * @return Templates used for linking text.
   */
  public List<String> getTemplatesForLinkingText() {
    if (templatesForLinkingText != null) {
      return new ArrayList<String>(templatesForLinkingText);
    }
    return null;
  }

  /* ================================================================================= */
  /* Wiktionary                                                                        */
  /* ================================================================================= */

  /**
   * Wiktionary interwiki.
   */
  private String wiktionaryInterwiki;

  private List<TemplateMatch> wiktionaryMatches;

  /**
   * @param value Wiktionary interwiki.
   */
  private void setWiktionaryInterwiki(String value) {
    this.wiktionaryInterwiki = nonEmptyString(value);
  }

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
   * @return Wiktionary interwiki.
   */
  public String getWiktionaryInterwiki() {
    return wiktionaryInterwiki;
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
   * @param value Value.
   * @return Value either null or not empty.
   */
  private String nonEmptyString(String value) {
    if ((value != null) && (value.trim().length() > 0)) {
      return value;
    }
    return null;
  }

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
