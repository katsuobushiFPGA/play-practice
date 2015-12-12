package controllers

import play.api.Logger
import play.api.data.validation.Constraints
import play.api.mvc.Action
import play.api.mvc.Security._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Future

/**
 * Created by hiroto on 15/11/24.
 */
case class User(name: String)
// in a Security trait

object LoggingAction extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = {
    Logger.info("Calling action")
    block(request)
  }
}
case class UserData(name:String,age:Int,email:String)
class  View extends Controller {
  val emailPattern =  Constraints.pattern("[\\w\\d_-]+@[\\w\\d_-]+\\.[\\w\\d._-]+".r)
  val userForm = Form (
    mapping(
      "name" -> nonEmptyText(minLength = 1,maxLength = 7),
      "age" -> number(min=0,max=100),
      "email" -> text.verifying(emailPattern)
    )(UserData.apply)(UserData.unapply)
  )
  object Authenticated extends AuthenticatedBuilder (req => getUserFromRequest(req))
  def getUserFromRequest(requestHeader: RequestHeader) = {
    Some("401")
  }
  def index = Action {
    Ok(views.html.index("message"))
  }

  def contact = Action {
    Ok(views.html.contact("Play List"))
  }

  def about = Action {
    Ok(views.html.about("About Us"))
  }

  def user(name: String) = Action {
    Ok(views.html.about(name))
  }

  def userform = Action {
    Ok(views.html.userform(userForm))
  }
  def submit = Action { implicit request =>
    userForm.bindFromRequest.fold(
      formWithErrors => {
        // binding Failures
        BadRequest(views.html.userform(userForm))
      },
      userData => {
        Ok(views.html.index(userForm.toString))
      }
    )
  }
  def responseTest = Action {
    request =>
    Ok("Hello,World")
      .withCookies(Cookie("theme","blue"))
      .withSession("connected" -> "user@mail.com")
  }
  def sessionRead = Action {
    request =>
      request.session.get("connected").map {
        user => Ok("Hello" + user)
      }.getOrElse {
        Unauthorized("Oops, you are not connected.")
      }
  }
  def sessionClear = Action {
    request =>
      Ok("clear").withNewSession.discardingCookies(DiscardingCookie("theme"))
  }
  def actionBuilderTest = LoggingAction {
      Ok("Hello World")
  }
  // then in a controller
  def authenticated = Authenticated { implicit request =>
    Ok("Hello " + request.user)
  }
  def futureGet = Action.async {
    import scala.concurrent.ExecutionContext.Implicits.global
    val futureInt = scala.concurrent.Future {
      val str = "aiueo"
      str.toLowerCase
    }
    futureInt.map(i=>Ok("Got Result:" + i))
  }

}
