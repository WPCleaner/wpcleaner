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

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Suggestion;
import org.wikipediacleaner.api.data.TemplateMatch;
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.api.data.TemplateMatcher1L;
import org.wikipediacleaner.api.data.TemplateMatcher1L2T;
import org.wikipediacleaner.api.data.TemplateMatcher1LT;


/**
 * Configuration for WPCleaner.
 */
public class WPCConfiguration {

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
    initDefaultEncyclopedicNamespaces();
  }

  /**
   * @param input Reader for general WPCleaner configuration.
   */
  public void setGeneralConfiguration(Reader input) throws APIException {
    templateMatchers = new HashMap<String, List<TemplateMatcher>>();
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
              (line.charAt(i) != '_')) {
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

    // Properties available also in user configuration
    if (name.equals("dab_comment")) {
      setDisambiguationComment(value);
    } else if (name.equals("dab_comment_1")) {
      setDisambiguationComment1(value);
    } else if (name.equals("dab_comment_todo")) {
      setDisambiguationCommentTodo(value);
    } else if (name.equals("dab_comment_todo_1")) {
      setDisambiguationCommentTodo1(value);
    } else if (name.equals("dab_warning_comment")) {
      setDisambiguationWarningComment(value);
    } else if (name.equals("dab_warning_comment_1")) {
      setDisambiguationWarningComment1(value);
    } else if (name.equals("dab_warning_comment_done")) {
      setDisambiguationWarningCommentDone(value);
    } else if (name.equals("general_suggestions")) {
      setSuggestionPages(value, general);
    } else if (name.equals("general_suggestions_typo")) {
      setSuggestionTypoPages(value, general);
    } else if (!general) {
      return;
    }

    // Properties available only in general configuration
    if (name.equals("help_url")) {
      setHelpURL(value);
    } else if (name.equals("help_page")) {
      setHelpPage(value);
    } else if (name.equals("general_pipe_template")) {
      setPipeTemplate(value);
    } else if (name.equals("general_encyclopedic_namespaces")) {
      setEncyclopedicNamespaces(value);
    } else if (name.equals("general_todo_templates")) {
      setTodoTemplates(value);
    } else if (name.equals("general_todo_link_templates")) {
      setTodoLinkTemplates(value);
    } else if (name.equals("general_todo_subpage")) {
      setTodoSubpage(value);
    } else if (name.equals("general_todo_subpage_force")) {
      setTodoSubpageForce(value);
    } else if (name.equals("general_todo_subpage_force_other")) {
      setTodoSubpageForceOther(value);
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
    } else if (name.equals("dab_warning_template")) {
      setDisambiguationWarningTemplate(value);
    } else if (name.equals("dab_warning_template_comment")) {
      setDisambiguationWarningTemplateComment(value);
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
    } else if (name.equals("translation_comment")) {
      setTranslationComment(value);
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
  /* Help                                                                              */
  /* ================================================================================= */

  /**
   * URL of the help page.
   */
  private String helpURL;

  /**
   * Help page.
   */
  private String helpPage;

  /**
   * @param value URL of the help page.
   */
  private void setHelpURL(String value) {
    this.helpURL = nonEmptyString(value);
  }

  /**
   * @param value Help page.
   */
  private void setHelpPage(String value) {
    this.helpPage = nonEmptyString(value);
  }

  /**
   * @return URL of the help page.
   */
  public String getHelpURL() {
    if (helpURL != null) {
      return helpURL;
    }
    return "http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation";
  }

  /**
   * @return Help page.
   */
  public String getHelpPage() {
    return helpPage;
  }

  /* ================================================================================= */
  /* General                                                                           */
  /* ================================================================================= */

  /**
   * Template creating a "|".
   */
  private String pipeTemplate;

  /**
   * Encyclopedic name spaces.
   */
  private List<Integer> encyclopedicNamespaces;

  /**
   * Encyclopedic talk name spaces.
   */
  private List<Integer> encyclopedicTalkNamespaces;

  /**
   * @param value Template creating a "|".
   */
  private void setPipeTemplate(String value) {
    this.pipeTemplate = nonEmptyString(value);
  }

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
   * @return Template creating a "|".
   */
  public String getPipeTemplate() {
    return pipeTemplate;
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
   * "To do" sub-page.
   */
  private String todoSubpage;

  /**
   * Force usage of "to do" sub-page in main name space.
   */
  private boolean todoSubpageForce;

  /**
   * Force usage of "to do" sub-page in other name spaces.
   */
  private boolean todoSubpageForceOther;

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
   * "To do" sub-page.
   */
  private void setTodoSubpage(String value) {
    this.todoSubpage = nonEmptyString(value);
  }

  /**
   * @param value Force usage of "to do" sub-page in main name space.
   */
  private void setTodoSubpageForce(String value) {
    this.todoSubpageForce = false;
    if (value != null) {
      this.todoSubpageForce = Boolean.parseBoolean(value);
    }
  }

  /**
   * @param value Force usage of "to do" sub-page in other name spaces.
   */
  private void setTodoSubpageForceOther(String value) {
    this.todoSubpageForceOther = false;
    if (value != null) {
      this.todoSubpageForceOther = Boolean.parseBoolean(value);
    }
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

  /**
   * @return "To do" sub-page.
   */
  public String getTodoSubpage() {
    return todoSubpage;
  }

  /**
   * @return Force usage of "to do" sub-page in main name space.
   */
  public boolean getTodoSubpageForce() {
    return todoSubpageForce;
  }

  /**
   * @return Force usage of "to do" sub-page in other name spaces.
   */
  public boolean getTodoSubpageForceOther() {
    return todoSubpageForceOther;
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
                PageAnalysis analysis = new PageAnalysis(page, contents);
                Collection<PageElementTemplate> templates = analysis.getTemplates(elements[1]);
                for (PageElementTemplate template : templates) {
                  String patternText = template.getParameterValue(elements[2]);
                  Suggestion suggestion = tmpMap.get(patternText);
                  if (suggestion == null) {
                    suggestion = Suggestion.createSuggestion(patternText);
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

        // Construct suggestions from AWB format
        if (suggestionTypoPages != null) {
          for (String suggestionPage : suggestionTypoPages) {
            Page page = pages.get(suggestionPage);
            if ((page != null) && (page.getContents() != null)) {
              String contents = page.getContents();
              PageAnalysis analysis = new PageAnalysis(page, contents);
              Collection<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_OTHER_TYPO);
              for (PageElementTag tag : tags) {
                Parameter word = tag.getParameter("word");
                Parameter find = tag.getParameter("find");
                Parameter replace = tag.getParameter("replace");
                if ((word != null) && (find != null) && (replace != null)) {
                  String wordValue = word.getValue();
                  String findValue = find.getValue();
                  String replaceValue = replace.getValue();
                  if ((wordValue != null) && (findValue != null) && (replaceValue != null)) {
                    while (findValue.startsWith("\\b")) {
                      findValue = findValue.substring(2);
                    }
                    while (findValue.endsWith("\\b")) {
                      findValue = findValue.substring(0, findValue.length() - 2);
                    }
                    boolean shouldUse = true;
                    if (findValue.contains("\\b") || findValue.contains("\\B")) {
                      shouldUse = false;
                    }
                    if (findValue.contains("(?<")) {
                      shouldUse = false;
                    }
                    if (findValue.contains("{{") || findValue.contains("}}")) {
                      shouldUse = false;
                    }
                    if (shouldUse) {
                      Suggestion suggestion = tmpMap.get(findValue);
                      if (suggestion == null) {
                        suggestion = Suggestion.createSuggestion(findValue);
                        if (suggestion != null) {
                          tmpMap.put(findValue, suggestion);
                        }
                      }
                      if (suggestion != null) {
                        suggestion.setComment("Typo AWB " + wordValue);
                        suggestion.addReplacement(replaceValue);
                      }
                    } else {
                      System.err.println("Rejecting " + wordValue + " : " + findValue);
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
  /* Translation                                                                       */
  /* ================================================================================= */

  /**
   * Comment used when translating.
   */
  private String translationComment;

  /**
   * @param value Comment used when translating.
   */
  private void setTranslationComment(String value) {
    this.translationComment = nonEmptyString(value);
  }

  /**
   * @return Comment used when translating.
   */
  public String getTranslationComment() {
    return translationComment;
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
   * Comment for disambiguation links that have been fixed.
   */
  private String disambiguationComment;

  /**
   * Comment for one disambiguation link that has been fixed.
   */
  private String disambiguationComment1;

  /**
   * Comment for disambiguation links that still need to be fixed.
   */
  private String disambiguationCommentTodo;

  /**
   * Comment for one disambiguation link that still need to be fixed.
   */
  private String disambiguationCommentTodo1;

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
   * @param value Comment for disambiguation links that have been fixed.
   */
  private void setDisambiguationComment(String value) {
    this.disambiguationComment = nonEmptyString(value);
  }

  /**
   * @param value Comment for one disambiguation link that has been fixed.
   */
  private void setDisambiguationComment1(String value) {
    this.disambiguationComment1 = nonEmptyString(value);
  }

  /**
   * @param value Comment for disambiguation links that still need to be fixed.
   */
  private void setDisambiguationCommentTodo(String value) {
    this.disambiguationCommentTodo = nonEmptyString(value);
  }

  /**
   * @param value Comment for one disambiguation link that still need to be fixed.
   */
  private void setDisambiguationCommentTodo1(String value) {
    this.disambiguationCommentTodo1 = nonEmptyString(value);
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
    if ((count == 1) &&
        (disambiguationComment1 != null) &&
        (disambiguationComment1.length() > 0)) {
      return disambiguationComment1;
    }
    if (disambiguationComment != null) {
      try {
        return MessageFormat.format(disambiguationComment, Integer.valueOf(count));
      } catch (IllegalArgumentException e) {
        //
      }
      return disambiguationComment;
    }
    return "";
  }

  /**
   * @param count Number of disambiguation links that still need to be fixed.
   * @return Comment for disambiguation links that still need to be fixed.
   */
  public String getDisambiguationCommentTodo(int count) {
    if ((count == 1) &&
        (disambiguationCommentTodo1 != null) &&
        (disambiguationCommentTodo1.length() > 0)) {
      return disambiguationCommentTodo1;
    }
    String tmp = disambiguationCommentTodo;
    if (tmp == null) {
      tmp = ", {0} to be fixed";
    }
    try {
      return MessageFormat.format(tmp, Integer.valueOf(count));
    } catch (IllegalArgumentException e) {
      //
    }
    return tmp;
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
   * Template for warning about disambiguation links in a page.
   */
  private String disambiguationWarningTemplate;

  /**
   * Comment for warning template about disambiguation links in a page.
   */
  private String disambiguationWarningTemplateComment;

  /**
   * List of templates that should be before the disambiguation warning.
   */
  private List<String> disambiguationWarningAfterTemplates;

  /**
   * Comment for warning about disambiguation links in a page.
   */
  private String disambiguationWarningComment;

  /**
   * Comment for warning about one disambiguation link in a page.
   */
  private String disambiguationWarningComment1;

  /**
   * Comment for telling that disambiguation links have been fixed.
   */
  private String disambiguationWarningCommentDone;

  /**
   * @param value Template for warning about disambiguation links in a page.
   */
  private void setDisambiguationWarningTemplate(String value) {
    this.disambiguationWarningTemplate = nonEmptyString(value);
  }

  /**
   * @param value Comment for warning template about disambiguation links in a page.
   */
  private void setDisambiguationWarningTemplateComment(String value) {
    this.disambiguationWarningTemplateComment = nonEmptyString(value);
  }

  /**
   * @param value List of templates that should be before the disambiguation warning.
   */
  private void setDisambiguationWarningAfterTemplates(String value) {
    this.disambiguationWarningAfterTemplates = convertPropertyToStringList(value);
  }

  /**
   * @param value Comment for warning about disambiguation links in a page.
   */
  private void setDisambiguationWarningComment(String value) {
    this.disambiguationWarningComment = nonEmptyString(value);
  }

  /**
   * @param value Comment for warning about one disambiguation link in a page.
   */
  private void setDisambiguationWarningComment1(String value) {
    this.disambiguationWarningComment1 = nonEmptyString(value);
  }

  /**
   * @param value Comment for telling that disambiguation links have been fixed.
   */
  private void setDisambiguationWarningCommentDone(String value) {
    this.disambiguationWarningCommentDone = nonEmptyString(value);
  }

  /**
   * @return Template for warning about disambiguation links in a page.
   */
  public String getDisambiguationWarningTemplate() {
    return disambiguationWarningTemplate;
  }

  /**
   * @return Comment for warning template about disambiguation links in a page.
   */
  public String getDisambiguationWarningTemplateComment() {
    return disambiguationWarningTemplateComment;
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
    if ((count == 1) &&
        (disambiguationWarningComment1 != null) &&
        (disambiguationWarningComment1.length() > 0)) {
      return disambiguationWarningComment1;
    }
    if (disambiguationWarningComment != null) {
      try {
        return MessageFormat.format(disambiguationWarningComment, Integer.valueOf(count));
      } catch (IllegalArgumentException e) {
        //
      }
      return disambiguationWarningComment;
    }
    return disambiguationWarningTemplate;
  }

  /**
   * @return Comment for telling that disambiguation links have been fixed.
   */
  public String getDisambiguationWarningCommentDone() {
    if (disambiguationWarningCommentDone != null) {
      return disambiguationWarningCommentDone;
    }
    return disambiguationWarningTemplate;
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
        tmp.add(DataManager.getPage(
            wiki,
            Namespace.getTitle(Namespace.TEMPLATE, wiki.getNamespaces(), template),
            null, null));
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
      if (page.equals(Namespace.getTitle(
          Namespace.TEMPLATE,
          wiki.getNamespaces(),
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
  private List<String> convertPropertyToStringList(String property) {
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
