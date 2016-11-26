/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * MediaWiki API message requests.
 */
public class ApiAllMessagesRequest extends ApiMetaRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Arguments.
   */
  public final static String PROPERTY_ARGS = "amargs";

  /**
   * Property for Customized.
   */
  public final static String PROPERTY_CUSTOMISED = "amcustomised";

  /**
   * Property for Enabling parser.
   */
  public final static String PROPERTY_ENABLE_PARSER = "amenableparser";

  /**
   * Property for Filter.
   */
  public final static String PROPERTY_FILTER = "amfilter";

  /**
   * Property for From.
   */
  public final static String PROPERTY_FROM = "amfrom";

  /**
   * Property for Including local messages.
   */
  public final static String PROPERTY_INCLUDE_LOCAL = "amincludelocal";

  /**
   * Property for Language.
   */
  public final static String PROPERTY_LANG = "amlang";

  /**
   * Property for Messages.
   */
  public final static String PROPERTY_MESSAGES = "ammessages";

  /**
   * Property for Disabling content.
   */
  public final static String PROPERTY_NO_CONTENT = "amnocontent";

  /**
   * Property for Prefix.
   */
  public final static String PROPERTY_PREFIX = "amprefix";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "amprop";

  /**
   * Property for Properties / Default.
   */
  public final static String PROPERTY_PROP_DEFAULT = "default";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "amtitle";

  /**
   * Property for To.
   */
  public final static String PROPERTY_TO = "amto";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiAllMessagesResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiAllMessagesRequest(EnumWikipedia wiki, ApiAllMessagesResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load one message.
   * 
   * @param messageName Name of the message.
   * @return Message.
   */
  public String loadMessage(
      String messageName) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_META,
        PROPERTY_META_ALLMESSAGES);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_MESSAGES, messageName);
    return result.executeMessage(properties);
  }

  /**
   * Load several messages.
   * 
   * @param messageNames Names of the messages.
   * @return Messages.
   */
  public Map<String, String> loadMessages(
      List<String> messageNames) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_META,
        PROPERTY_META_ALLMESSAGES);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_MESSAGES, constructList(messageNames));
    Map<String, String> messages = new HashMap<>();
    while (result.executeMessages(properties, messages)) {
      // Nothing to do
    }
    return messages;
  }
}
