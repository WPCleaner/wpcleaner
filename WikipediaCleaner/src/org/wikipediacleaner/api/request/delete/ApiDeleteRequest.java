/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.delete;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.impl.CommentDecorator;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API delete requests.
 */
public class ApiDeleteRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Old image.
   */
  public final static String PROPERTY_OLDIMAGE = "oldimage";

  /**
   * Property for Page identifier.
   */
  public final static String PROPERTY_PAGEID = "pageid";

  /**
   * Property for Reason.
   */
  public final static String PROPERTY_REASON = "reason";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "title";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "token";

  /**
   * Property for Unwatch.
   */
  public final static String PROPERTY_UNWATCH = "unwatch";

  /**
   * Property for Watch.
   */
  public final static String PROPERTY_WATCH = "watch";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiDeleteResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiDeleteRequest(EnumWikipedia wiki, ApiDeleteResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Delete one page.
   * 
   * @param page Page to be deleted.
   * @param reason Reason for deletion.
   * @param automatic True if the modification is automatic.
   */
  public void deletePage(
      Page page, String reason,
      boolean automatic) throws APIException {
    Map<String, String> properties = getProperties(ACTION_DELETE, result.getFormat());
    if (reason != null) {
      properties.put(PROPERTY_REASON, reason);
    }
    properties.put(PROPERTY_TITLE, page.getTitle());
    properties.put(PROPERTY_TOKEN, getWiki().getConnection().getDeleteToken());
    CommentDecorator decorator = getWiki().getCommentDecorator();
    if (decorator != null) {
      decorator.manageComment(properties, PROPERTY_REASON, "tags", automatic);
    }
    result.executeDelete(properties);
  }
}
