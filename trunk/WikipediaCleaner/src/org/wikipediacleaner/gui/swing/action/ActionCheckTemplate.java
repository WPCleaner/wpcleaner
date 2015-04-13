/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JMenuItem;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateData;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * Manage actions for checking a template.
 */
public class ActionCheckTemplate implements ActionListener {

  /**
   * Create a menu item for checking a template.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param template Template.
   * @return Menu item.
   */
  public static JMenuItem createMenuItem(
      Component parent,
      EnumWikipedia wiki, PageElementTemplate template) {
    JMenuItem menuItem = Utilities.createJMenuItem(GT._("Check template"), true);
    menuItem.addActionListener(new ActionCheckTemplate(parent, wiki, template));
    return menuItem;
  }
  /**
   * Parent component.
   */
  private final Component parent;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * Template to be checked.
   */
  private final PageElementTemplate template;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param template Template to be checked.
   */
  private ActionCheckTemplate(
      Component parent,
      EnumWikipedia wiki, PageElementTemplate template) {
    this.parent = parent;
    this.wiki = wiki;
    this.template = template;
  }

  /**
   * Check a template.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (wiki == null) || (template == null)) {
      return;
    }

    // Retrieve TemplateData
    API api = APIFactory.getAPI();
    String title = wiki.getWikiConfiguration().getPageTitle(
        Namespace.TEMPLATE, template.getTemplateName());
    Page templatePage = DataManager.getPage(
        wiki, title, null, null, null);
    TemplateData templateData = null;
    try {
      templateData = api.retrieveTemplateData(wiki, templatePage);
    } catch (APIException exception) {
      return;
    }

    // Inform user if no template data has been found
    if (templateData == null) {
      String message = GT._(
          "Template \"{0}\" has no {1} block defined.",
          new Object[] { template.getTemplateName(), "TemplateData" });
      Utilities.displayWarning(parent, message);
      return;
    }

    // Perform analysis
    List<String> warnings = new ArrayList<String>();

    // Check each parameter defined in TemplateData.
    for (TemplateData.Parameter param : templateData.getParameters()) {
      String aliases = listAliases(param);

      // Find parameters matching the TemplateData parameter.
      List<PageElementTemplate.Parameter> parameters = new ArrayList<PageElementTemplate.Parameter>();
      for (int i = 0; i < template.getParameterCount(); i++) {
        PageElementTemplate.Parameter parameter = template.getParameter(i);
        if (param.isPossibleName(parameter.getComputedName())) {
          parameters.add(parameter);

          // Check parameter type
          if (param.getType() != null) {
            boolean ok = true;
            TemplateData.EnumParameterType enumType = param.getType();
            String value = parameter.getValue();
            switch (enumType) {
            case STRING:
            case WIKI_PAGE_NAME:
            case WIKI_USER_NAME:
            case UNKNOWN:
              // Nothing to check;
              break;

            case NUMBER:
              if (value != null) {
                for (int charNum = 0; charNum < value.length(); charNum++) {
                  char currentChar = value.charAt(charNum);
                  if (!Character.isDigit(currentChar) &&
                      !Character.isWhitespace(currentChar) &&
                      (currentChar != '.')) {
                    ok = false;
                  }
                }
              }
              break;
            }
            if (!ok) {
              if (aliases != null) {
                warnings.add(GT._(
                    "Parameter defined as \"{0}\" (aliases {1}) in {2} should be of type \"{3}\", but actual value is \"{4}\".",
                    new Object[] { param.getName(), aliases, "TemplateData", enumType.toString(), value }));
              } else {
                warnings.add(GT._(
                    "Parameter defined as \"{0}\" in {1} should be of type \"{2}\", but actual value is \"{3}\".",
                    new Object[] { param.getName(), "TemplateData", enumType.toString(), value }));
              }
            }
          }
        }
      }

      // Check mandatory parameters
      if (param.isRequired() && parameters.isEmpty()) {
        if (aliases != null) {
          warnings.add(GT._(
              "Parameter defined as \"{0}\" (aliases {1}) in {2} is required, but it''s missing.",
              new Object[] { param.getName(), aliases, "TemplateData" }));
        } else {
          warnings.add(GT._(
              "Parameter defined as \"{0}\" in {1} is required, but it''s missing.",
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
          warnings.add(GT._(
              "Parameter defined as \"{0}\" (aliases {1}) in {2} is present several times: {3}.",
              new Object[] { param.getName(), aliases, "TemplateData", buffer.toString() }));
        } else {
          warnings.add(GT._(
              "Parameter defined as \"{0}\" in {1} is present several times: {2}.",
              new Object[] { param.getName(), "TemplateData", buffer.toString() }));
        }
      }
    }

    // Check each parameter
    for (int i = 0; i < template.getParameterCount(); i++) {
      PageElementTemplate.Parameter parameter = template.getParameter(i);
      TemplateData.Parameter param = templateData.getParameter(parameter.getComputedName());
      if (param == null) {
        warnings.add(GT._(
            "Parameter \"{0}\" is not defined in {1}.",
            new Object[] { parameter.getComputedName(), "TemplateData" }));
      }
    }

    // Report result
    if (warnings.isEmpty()) {
      String message = GT._(
          "No problem has been detected with template \"{0}\".",
          template.getTemplateName());
      Utilities.displayInformationMessage(parent, message);
      return;
    }
    StringBuilder buffer = new StringBuilder();
    buffer.append(GT.__(
        "The following problem has been detected with template \"{0\":",
        "The following problems have been detected with template \"{0}\":",
        warnings.size(),
        template.getTemplateName()));
    for (String warning : warnings) {
      buffer.append("\n* ");
      buffer.append(warning);
    }
    Utilities.displayWarning(parent, buffer.toString());
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
}
