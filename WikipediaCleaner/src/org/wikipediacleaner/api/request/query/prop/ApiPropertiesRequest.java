/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * Base class MediaWiki API properties query requests.
 */
public class ApiPropertiesRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Generator prefix for properties.
   */
  public final static String GENERATOR_PREFIX = "g";

  /**
   * Property for Generator.
   */
  public final static String PROPERTY_GENERATOR = "generator";

  /**
   * Property for Page identifiers.
   */
  public final static String PROPERTY_PAGEIDS = "pageids";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "prop";

  /**
   * Property value for Properties / Categories.
   */
  public final static String PROPERTY_PROP_CATEGORIES = "categories";

  /**
   * Property value for Properties / Information.
   */
  public final static String PROPERTY_PROP_INFO = "info";

  /**
   * Property value for Properties / Language links.
   */
  public final static String PROPERTY_PROP_LANGLINKS = "langlinks";

  /**
   * Property value for Properties / Links.
   */
  public final static String PROPERTY_PROP_LINKS = "links";

  /**
   * Property value for Properties / Page properties.
   */
  public final static String PROPERTY_PROP_PAGEPROPS = "pageprops";

  /**
   * Property value for Properties / Revisions.
   */
  public final static String PROPERTY_PROP_REVISIONS = "revisions";

  /**
   * Property value for Properties / Templates.
   */
  public final static String PROPERTY_PROP_TEMPLATES = "templates";

  /**
   * Property for Redirects.
   */
  public final static String PROPERTY_REDIRECTS = "redirects";

  /**
   * Property for Revision identifiers.
   */
  public final static String PROPERTY_REVIDS = "revids";

  /**
   * Property for Titles.
   */
  public final static String PROPERTY_TITLES = "titles";

  // ==========================================================================
  // Wiki management
  // ==========================================================================

  /**
   * Base constructor.
   * 
   * @param wiki Wiki.
   */
  protected ApiPropertiesRequest(EnumWikipedia wiki) {
    super(wiki);
  }
}
