/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.parse;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Section;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API parse requests.
 */
public class ApiParseRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for OnlyPST (transform before save, but no analysis)
   */
  public final static String PROPERTY_ONLY_PST = "onlypst";

  /**
   * Property for Page.
   */
  public final static String PROPERTY_PAGE = "page";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "prop";

  /**
   * Property value for Properties / Categories.
   */
  public final static String PROPERTY_PROPERTIES_CATEGORIES = "categories";

  /**
   * Property value for Properties / External links.
   */
  public final static String PROPERTY_PROPERTIES_EXTERNAL_LINKS = "externallinks";

  /**
   * Property value for Properties / Images.
   */
  public final static String PROPERTY_PROPERTIES_IMAGES = "images";

  /**
   * Property value for Properties / Language links.
   */
  public final static String PROPERTY_PROPERTIES_LANG_LINKS = "langlinks";

  /**
   * Property value for Properties / Links.
   */
  public final static String PROPERTY_PROPERTIES_LINKS = "links";

  /**
   * Property value for Properties / Revision Id.
   */
  public final static String PROPERTY_PROPERTIES_REVISION_ID = "revid";

  /**
   * Property value for Properties / Sections.
   */
  public final static String PROPERTY_PROPERTIES_SECTIONS = "sections";

  /**
   * Property value for Properties / Templates.
   */
  public final static String PROPERTY_PROPERTIES_TEMPLATES = "templates";

  /**
   * Property value for Properties / Text.
   */
  public final static String PROPERTY_PROPERTIES_TEXT = "text";

  /**
   * Property for Text.
   */
  public final static String PROPERTY_TEXT = "text";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "title";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiParseResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiParseRequest(EnumWikipedia wiki, ApiParseResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Parse.
   * 
   * @param title Page title.
   * @param text Page contents.
   * @param full True to do a full parsing.
   * @return Parsed text.
   * @throws APIException
   */
  public String parseText(String title, String text, boolean full) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PARSE, result.getFormat());
    properties.put(PROPERTY_TITLE, title);
    properties.put(PROPERTY_TEXT, text);
    properties.put(PROPERTY_PROPERTIES, PROPERTY_PROPERTIES_TEXT);
    if (!full) {
      properties.put(PROPERTY_ONLY_PST, "");
    }
    return result.executeParse(properties);
  }

  /**
   * Retrieve list of sections.
   * 
   * @param page Page (Revision Id is update on output).
   * @return List of sections in the page.
   * @throws APIException
   */
  public List<Section> retrieveSections(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PARSE, result.getFormat());
    properties.put(PROPERTY_PAGE, page.getTitle());
    properties.put(
        PROPERTY_PROPERTIES,
        PROPERTY_PROPERTIES_REVISION_ID + "|" + PROPERTY_PROPERTIES_SECTIONS);
    return result.executeSections(page, properties);
  }
}
