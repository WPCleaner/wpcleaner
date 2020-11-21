/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.contents.tag.FullTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.TagBuilder;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Handle a report about modifications.
 */
public class ModificationReport {

  /** List of modifications */
  private final List<Modification> modifications;

  /** List of errors */
  private final List<Error> errors;

  /**
   * Initialize a report.
   */
  public ModificationReport() {
    this.modifications = new ArrayList<>();
    this.errors = new ArrayList<>();
  }

  /**
   * @param modification Modification.
   */
  public void addModification(Modification modification) {
    if (modification != null) {
      modifications.add(modification);
    }
  }

  /**
   * @param error Error.
   */
  public void addError(Error error) {
    if (error != null) {
      errors.add(error);
    }
  }

  /**
   * @param wiki Wiki.
   * @return Textual description of the report.
   */
  public String getReport(EnumWikipedia wiki) {
    StringBuilder result = new StringBuilder();
    Configuration config = Configuration.getConfiguration();
    boolean secured = config.getBoolean(null, ConfigurationValueBoolean.SECURE_URL);

    // Report modifications
    for (Modification modification : modifications) {

      // Page
      String title = modification.getTitle();
      String formattedTitle = FullTagBuilder.from(PageElementTag.TAG_HTML_A, title)
          .addAttribute("href", wiki.getSettings().getURL(title, false, secured))
          .toString();
      result.append(
          GT._T("Page {0}:", formattedTitle));
      result.append("\n");
      result.append(TagBuilder.UL_OPEN);
      result.append("\n");

      // List of modifications
      for (String replacement : modification.getModifications()) {
        result.append(FullTagBuilder.from(
            PageElementTag.TAG_HTML_LI,
            replacement.replaceAll("\\&", "&amp;").replaceAll("\\<", "&lt;"))).toString();
      }

      result.append(TagBuilder.UL_CLOSE);
      result.append("\n");
    }

    // Report errors
    if (errors.size() > 0) {
      result.append("\n\n");
      result.append(FullTagBuilder
          .from(PageElementTag.TAG_HTML_FONT, GT._T("The following errors have occurred:"))
          .addAttribute("color", "red").toString());
      result.append(TagBuilder.UL_OPEN);
      result.append("\n");
      for (Error error: errors) {
        result.append(TagBuilder.LI_OPEN);
        String title = error.getTitle();
        String formattedTitle = FullTagBuilder.from(PageElementTag.TAG_HTML_A, title)
            .addAttribute("href", wiki.getSettings().getURL(title, false, secured)).toString();
        result.append(
            GT._T("Page {0}:", formattedTitle));
        result.append(" ");
        result.append(error.getError().getCode());
        result.append(TagBuilder.LI_CLOSE);
        result.append("\n");
      }
      result.append(TagBuilder.UL_CLOSE);
      result.append("\n");
    }

    return result.toString();
  }

  /**
   * Bean for holding modifications done to a page.
   */
  public static class Modification {

    /** Page that has been modified */
    private final String title;

    /** List of modifications done to the page */
    private final List<String> modifications;

    /**
     * @param title Page title.
     */
    public Modification(String title) {
      this.title = title;
      this.modifications = new ArrayList<>();
    }

    /**
     * @param modification Modification done to the page.
     */
    public void addModification(String modification) {
      if (modification != null) {
        modifications.add(modification);
      }
    }

    /**
     * @return Title of the page.
     */
    String getTitle() {
      return title;
    }

    /**
     * @return List of modifications done to the page.
     */
    List<String> getModifications() {
      return modifications;
    }
  }

  /**
   * Bean for holding errors.s
   */
  public static class Error {

    /** Page for which the error occurred */
    private final String title;

    /** Error which occurred */
    private final EnumQueryResult error;

    /**
     * @param title Page title.
     * @param error Error.
     */
    public Error(String title, EnumQueryResult error) {
      this.title = title;
      this.error = error;
    }

    /**
     * @return Page title.
     */
    String getTitle() {
      return title;
    }

    /**
     * @return Error.
     */
    EnumQueryResult getError() {
      return error;
    }
  }
}
