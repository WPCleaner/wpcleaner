/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;


/**
 * Generic API Exception.
 */
public class CaptchaException extends Exception {

  /**
   * 
   */
  private static final long serialVersionUID = 6106358257321983463L;

  /**
   * Captcha type.
   */
  private final String type;

  /**
   * Mime type.
   */
  private String mime;

  /**
   * Captcha Id.
   */
  private String id;

  /**
   * Captcha question.
   */
  private String question;

  /**
   * Captcha URL.
   */
  private String url;

  /**
   * Constructor.
   */
  public CaptchaException() {
    super();
    type = null;
  }

  /**
   * @param message Exception message.
   */
  public CaptchaException(String message) {
    super(message);
    this.type = null;
  }

  /**
   * @param message Exception message.
   * @param type Captcha type.
   */
  public CaptchaException(String message, String type) {
    super(message);
    this.type = type;
  }

  /**
   * @param cause Exception cause.
   */
  public CaptchaException(Throwable cause) {
    super(cause);
    this.type = null;
  }

  /**
   * @param cause Exception cause.
   * @param type Captcha type.
   */
  public CaptchaException(Throwable cause, String type) {
    super(cause);
    this.type = type;
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   */
  public CaptchaException(String message, Throwable cause) {
    super(message, cause);
    this.type = null;
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   * @param type Captcha type.
   */
  public CaptchaException(String message, Throwable cause, String type) {
    super(message, cause);
    this.type = null;
  }

  /**
   * @return Captcha type.
   */
  public String getType() {
    return type;
  }

  /**
   * @param mime Mime type.
   */
  public void setMime(String mime) {
    this.mime = mime;
  }

  /**
   * @return Mime type.
   */
  public String getMime() {
    return mime;
  }

  /**
   * @param id Captcha id.
   */
  public void setId(String id) {
    this.id = id;
  }

  /**
   * @return Captcha id.
   */
  public String getId() {
    return id;
  }

  /**
   * @param question Captcha question.
   */
  public void setQuestion(String question) {
    this.question = question;
  }

  /**
   * @return Captcha question.
   */
  public String getQuestion() {
    return question;
  }

  /**
   * @param url Captcha URL.
   */
  public void setURL(String url) {
    this.url = url;
  }

  /**
   * @return Captcha URL.
   */
  public String getURL() {
    return url;
  }
}
