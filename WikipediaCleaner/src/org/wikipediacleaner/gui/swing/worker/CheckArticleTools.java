/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateData;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for checking articles.
 */
public class CheckArticleTools {

  /** Wiki. */
  private final EnumWikipedia wiki;

  /** All reports. */
  private final List<CheckArticleReport> reports;

  /** Current report. */
  private CheckArticleReport currentReport;

  public CheckArticleTools(EnumWikipedia wiki) {
    this.wiki = wiki;
    reports = new ArrayList<>();
  }

  // ==========================================================================
  // Check
  // ==========================================================================

  /**
   * @param page Page to be checked.
   * @throws APIException Exception thrown by the API.
   */
  public void checkArticle(Page page)
      throws APIException {
    API api = APIFactory.getAPI();
    api.retrieveContents(wiki, Collections.singletonList(page), false, false);
    checkArticle(page, page.getContents());
  }

  /**
   * @param page Page to be checked.
   * @param contents Current contents.
   * @throws APIException Exception thrown by the API.
   */
  public void checkArticle(Page page, String contents)
      throws APIException {
    initCurrentReport(page, contents);
    checkTemplates();
  }

  /**
   * @param template Template.
   * @throws APIException Exception thrown by the API.
   */
  public void checkTemplate(PageElementTemplate template)
      throws APIException {
    initCurrentReport(null, null);
    CheckArticleStep step = currentReport.setCurrentStep(null);
    TemplateData templateData = retrieveTemplateData(template);
    checkTemplate(step, template, templateData, false);
  }

  // ==========================================================================
  // Templates analysis
  // ==========================================================================

  /**
   * Check templates in the current page.
   * 
   * @throws APIException Exception thrown by the API.
   */
  private void checkTemplates() throws APIException {

    PageAnalysis analysis = currentReport.getAnalysis();
    List<PageElementTemplate> templates = analysis.getTemplates();
    if (templates == null) {
      return;
    }
    CheckArticleStep step = currentReport.setCurrentStep(GT._T("Templates"));

    // Check the existence of TemplateData blocks for every template
    CheckArticleElement element = step.setCurrentElement(GT._T("Existence of {0} blocks", "TemplateData"));
    Map<String, TemplateData> templateDataMap = new HashMap<>();
    Map<String, TemplateData> backupTemplateDataMap = new HashMap<>();
    for (PageElementTemplate template : templates) {
      String templateName = template.getTemplateName();
      if (!templateDataMap.containsKey(templateName)) {
        TemplateData templateData = retrieveTemplateData(template);
        templateDataMap.put(templateName, templateData);
        if (templateData == null) {
          String message = GT._T(
              "Template \"{0}\" has no {1} block defined.",
              new Object[] { template.getTemplateName(), "TemplateData" });
          element.addWarning(message);
          templateData = computeTemplateData(template);
          if (templateData != null) {
            backupTemplateDataMap.put(templateName, templateData);
          }
        }
      }
    }

    // Check each template
    for (PageElementTemplate template : templates) {
      TemplateData templateData = templateDataMap.get(template.getTemplateName());
      if (templateData != null) {
        checkTemplate(step, template, templateData, false);
      } else {
        templateData = backupTemplateDataMap.get(template.getTemplateName());
        if (templateData != null) {
          checkTemplate(step, template, templateData, true);
        }
      }
    }
    // Clean up
    step.cleanup();
  }

  /**
   * Retrieve TemplateData.
   * 
   * @param template Template.
   * @return TemplateData.
   * @throws APIException Exception thrown by the API.
   */
  private TemplateData retrieveTemplateData(
      PageElementTemplate template) throws APIException {
    String templateName = template.getTemplateName();
    String title = wiki.getWikiConfiguration().getPageTitle(
        Namespace.TEMPLATE, templateName);
    Page templatePage = DataManager.getPage(
        wiki, title, null, null, null);
    API api = APIFactory.getAPI();
    return api.retrieveTemplateData(wiki, templatePage);
  }

  /**
   * Compute TemplateData.
   * 
   * @param template Template.
   * @return TemplateData.
   * @throws APIException Exception thrown by the API.
   */
  private TemplateData computeTemplateData(
      PageElementTemplate template) throws APIException {
    String templateName = template.getTemplateName();
    String title = wiki.getWikiConfiguration().getPageTitle(
        Namespace.TEMPLATE, templateName);
    Page templatePage = DataManager.getPage(
        wiki, title, null, null, null);
    API api = APIFactory.getAPI();
    api.retrieveContents(wiki, Collections.singletonList(templatePage), false, true);
    return TemplateData.createFromContent(
        templatePage.getAnalysis(templatePage.getContents(), false));
  }

  /**
   * @param step Current step.
   * @param template Template.
   * @param templateData Template data.
   * @param correct True if the template data is generated.
   */
  private void checkTemplate(
      CheckArticleStep step,
      PageElementTemplate template,
      TemplateData templateData, boolean generated) {

    String templateName = template.getTemplateName();
    String message = GT._T(
        "Template \"{0}\" ({1}-{2})",
        new Object[] { templateName, template.getBeginIndex(), template.getEndIndex() });
    CheckArticleElement element = step.setCurrentElement(message);

    // Check TemplateData
    if (templateData == null) {
      String warning = GT._T(
          "Template \"{0}\" has no {1} block defined.",
          new Object[] { template.getTemplateName(), "TemplateData" });
      element.addWarning(warning);
      return;
    }

    // Check each parameter defined in TemplateData.
    for (TemplateData.Parameter param : templateData.getParameters()) {
      String aliases = listAliases(param);

      // Find parameters matching the TemplateData parameter.
      List<PageElementTemplate.Parameter> parameters = new ArrayList<>();
      for (int i = 0; i < template.getParameterCount(); i++) {
        PageElementTemplate.Parameter parameter = template.getParameter(i);
        if (param.isPossibleName(parameter.getComputedName())) {
          parameters.add(parameter);

          // Check parameter type
          if (param.getType() != null) {
            TemplateData.EnumParameterType enumType = param.getType();
            if (!enumType.isCompatible(parameter.getStrippedValue())) {
              String value = parameter.getValue();
              if (aliases != null) {
                element.addWarning(GT._T(
                    "Parameter defined as \"{0}\" (aliases {1}) in {2} should be of type \"{3}\", but actual value is \"{4}\".",
                    new Object[] { param.getName(), aliases, "TemplateData", enumType.toString(), value }));
              } else {
                element.addWarning(GT._T(
                    "Parameter defined as \"{0}\" in {1} should be of type \"{2}\", but actual value is \"{3}\".",
                    new Object[] { param.getName(), "TemplateData", enumType.toString(), value }));
              }
            }
          }

          // Check deprecated parameter
          if (param.isDeprecated()) {
            String value = parameter.getValue();
            if (aliases != null) {
              message = GT._T(
                  "Parameter defined as \"{0}\" (aliases {1}) in {2} is deprecated, but still present with value \"{3}\".",
                  new Object[] { param.getName(), aliases, "TemplateData", value });
            } else {
              message = GT._T(
                  "Parameter defined as \"{0}\" in {1} is deprecated, but still present with value \"{2}\".",
                  new Object[] { param.getName(), "TemplateData", value });
            }
            if (param.getDeprecatedText() != null) {
              message += " (" + param.getDeprecatedText() + ")";
            }
            element.addWarning(message);
          }
        }
      }

      // Check mandatory parameters
      if (param.isRequired() && parameters.isEmpty()) {
        if (aliases != null) {
          element.addWarning(GT._T(
              "Parameter defined as \"{0}\" (aliases {1}) in {2} is required, but is missing.",
              new Object[] { param.getName(), aliases, "TemplateData" }));
        } else {
          element.addWarning(GT._T(
              "Parameter defined as \"{0}\" in {1} is required, but is missing.",
              new Object[] { param.getName(), "TemplateData" }));
        }
      }

      // Check duplicate parameters
      if (parameters.size() > 1) {
        StringBuilder buffer = new StringBuilder();
        for (PageElementTemplate.Parameter parameter : parameters) {
          if (buffer.length() > 0) {
            buffer.append(", ");
          }
          buffer.append(parameter.getComputedName());
        }
        if (aliases != null) {
          element.addWarning(GT._T(
              "Parameter defined as \"{0}\" (aliases {1}) in {2} is present several times: {3}.",
              new Object[] { param.getName(), aliases, "TemplateData", buffer.toString() }));
        } else {
          element.addWarning(GT._T(
              "Parameter defined as \"{0}\" in {1} is present several times: {2}.",
              new Object[] { param.getName(), "TemplateData", buffer.toString() }));
        }
      }
    }

    // Check each parameter
    if (!generated) {
      for (int i = 0; i < template.getParameterCount(); i++) {
        PageElementTemplate.Parameter parameter = template.getParameter(i);
        TemplateData.Parameter param = templateData.getParameter(parameter.getComputedName());
        if (param == null) {
          element.addWarning(GT._T(
              "Parameter \"{0}\" is not defined in {1}.",
              new Object[] { parameter.getComputedName(), "TemplateData" }));
        }
      }
    }
  }

  /**
   * @param param TemplateData parameter.
   * @return String representation of parameter aliases.
   */
  private String listAliases(TemplateData.Parameter param) {
    if (param == null) {
      return null;
    }
    List<String> aliases = param.getAliases();
    if ((aliases == null) || aliases.isEmpty()) {
      return null;
    }
    StringBuilder bufferAliases = new StringBuilder();
    for (String alias : aliases) {
      if (bufferAliases.length() > 0) {
        bufferAliases.append(", ");
      }
      bufferAliases.append(alias);
    }
    return bufferAliases.toString();
  }

  // ==========================================================================
  // Report management
  // ==========================================================================

  /**
   * @param page Page to be checked.
   */
  private void initCurrentReport(Page page, String contents) {
    currentReport = new CheckArticleReport(page, contents);
    reports.add(currentReport);
  }

  /**
   * @return True if a problem is reported.
   */
  public boolean isProblemReported() {
    for (CheckArticleReport report : reports) {
      if (report.hasWarnings()) {
        return true;
      }
    }
    return false;
  }

  /**
   * @return Full report.
   */
  public String getReport() {
    StringBuilder result = new StringBuilder();

    // Detail each report
    for (CheckArticleReport report : reports) {
      String reportTitle = report.getTitle();
      String reportExtra = "";
      if (reportTitle != null) {
        reportExtra = "=";
        result.append("== ");
        result.append(reportTitle);
        result.append(" ==\n");
      }

      if (!report.hasWarnings()) {
        result.append(GT._T("No problem has been detected."));
        result.append("\n\n");
      } else {
        // Detail each step
        for (CheckArticleStep step : report.getSteps()) {
          String stepTitle = step.getTitle();
          String stepExtra = "";
          if (stepTitle != null) {
            stepExtra = "=";
            result.append(reportExtra);
            result.append("== ");
            result.append(stepTitle);
            result.append(" ==");
            result.append(reportExtra);
            result.append("\n");
          }

          if (!step.hasWarnings()) {
            result.append(GT._T("No problem has been detected."));
            result.append("\n\n");
          } else {
            // Detail each element
            for (CheckArticleElement element : step.getElements()) {
              String elementTitle = element.getTitle();
              if (elementTitle != null) {
                result.append(reportExtra);
                result.append(stepExtra);
                result.append("== ");
                result.append(elementTitle);
                result.append(" ==");
                result.append(stepExtra);
                result.append(reportExtra);
                result.append("\n");
              }

              if (!element.hasWarnings()) {
                result.append(GT._T("No problem has been detected."));
                result.append("\n\n");
              } else {
                List<String> warnings = element.getWarnings();
                result.append(GT.__(
                    "The following problem has been detected:",
                    "The following problems have been detected:",
                    warnings.size(),
                    (Object[]) null));
                result.append("\n");
                for (String warning : warnings) {
                  result.append("* ");
                  result.append(warning);
                  result.append("\n");
                }
                result.append("\n");
              }
            }
          }
        }
      }
    }

    return result.toString();
  }

  // ==========================================================================
  // Reports
  // ==========================================================================

  /**
   * Utility class to store the results of checking an article.
   */
  private static class CheckArticleReport {

    /** Report title. */
    private final String title;

    /** Page analysis. */
    private final PageAnalysis analysis;

    /** Steps in page check. */
    private final List<CheckArticleStep> steps;

    /** Current stage in page check. */
    private CheckArticleStep currentStep;

    /**
     * @param page Checked page.
     * @param contents Page contents.
     */
    public CheckArticleReport(Page page, String contents) {
      this.title = (page != null) ? ("[[:" + page.getTitle() + "]]") : null;
      this.analysis = (page != null) ?
          ((contents != null) ?
              page.getAnalysis(contents, false) :
              page.getAnalysis(page.getContents(), false)) :
          null;
      this.steps = new ArrayList<>();
    }

    /**
     * @return Report title
     */
    public String getTitle() {
      return title;
    }

    /**
     * @return Page analysis.
     */
    public PageAnalysis getAnalysis() {
      return analysis;
    }

    /**
     * @param step Current step description.
     */
    public CheckArticleStep setCurrentStep(String step) {
      currentStep = new CheckArticleStep(step);
      steps.add(currentStep);
      return currentStep;
    }

    /**
     * @return List of steps.
     */
    public List<CheckArticleStep> getSteps() {
      return steps;
    }

    /**
     * @return True if a problem is reported.
     */
    public boolean hasWarnings() {
      for (CheckArticleStep step : steps) {
        if (step.hasWarnings()) {
          return true;
        }
      }
      return false;
    }
  }

  /**
   * Utility class to store the results of a step in checking an article. 
   */
  private static class CheckArticleStep {

    /** Step title. */
    private final String title;

    /** Elements checked. */
    private final List<CheckArticleElement> elements;

    /** Current element. */
    private CheckArticleElement currentElement;

    /**
     * @param title Step title.
     */
    public CheckArticleStep(String title) {
      this.title = title;
      this.elements = new ArrayList<>();
    }

    /**
     * @return Step title.
     */
    public String getTitle() {
      return title;
    }

    /**
     * @param element Current element description.
     */
    public CheckArticleElement setCurrentElement(String element) {
      currentElement = new CheckArticleElement(element);
      elements.add(currentElement);
      return currentElement;
    }

    /**
     * @return List of elements.
     */
    public List<CheckArticleElement> getElements() {
      return elements;
    }

    /**
     * @return True if a problem is reported.
     */
    public boolean hasWarnings() {
      for (CheckArticleElement element : elements) {
        if (element.hasWarnings()) {
          return true;
        }
      }
      return false;
    }

    /**
     * Clean up step for unnecessary elements.
     */
    public void cleanup() {
      currentElement = null;
      Iterator<CheckArticleElement> itElement = elements.iterator();
      while (itElement.hasNext()) {
        CheckArticleElement element = itElement.next();
        if (!element.hasWarnings()) {
          itElement.remove();
        }
      }
    }
  }

  /**
   * Utility class to store the results for an element in checking an article.
   */
  private static class CheckArticleElement {

    /** Element title. */
    private final String title;

    /** List of warnings. */
    private final List<String> warnings;

    /**
     * @param title Title of the element.
     */
    public CheckArticleElement(String title) {
      this.title = title;
      this.warnings = new ArrayList<>();
    }

    /**
     * @return Element title.
     */
    public String getTitle() {
      return title;
    }

    /**
     * @return List of warnings.
     */
    public List<String> getWarnings() {
      return warnings;
    }

    /**
     * @param warning Warning.
     */
    public void addWarning(String warning) {
      if ((warning != null) && (warning.trim().length() > 0)) {
        warnings.add(warning.trim());
      }
    }

    /**
     * @return True if element has warnings.
     */
    public boolean hasWarnings() {
      return !warnings.isEmpty();
    }
  }
}
