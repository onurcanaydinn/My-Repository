#include "olcSimpleEngine.h"
#include "vector"
#include "algorithm"

using namespace std;

class Player;

class Game;
class BlackHole;


class Ball :public enable_shared_from_this<Ball>
{
protected:

	bool isdead;
	double xcor, ycor;

	double mass;
	olc::Pixel color;

	Game* x;
public:

	void massgrow(double parameter) {
		mass += parameter;
	}
	void makeitbigger(shared_ptr<Ball> b) {
		mass += b->mass / 2;
	}
	
	void makeitdead() {
		isdead = true;
	}
	
	void useitwhencreatingrandom(double Time);
	virtual void checkifitisalive();
	Ball(double x, double y, double m, olc::Pixel c, Game* g) :xcor(x), ycor(y), mass(m), color(c), x(g), isdead(false) {}
	virtual void gotoblackhole(double x, double y, float fElapsedTime);
	virtual void Update(float fElapsedTime);
	virtual shared_ptr<Player> Divide(); //
	virtual void drawball();
	bool iscollided(shared_ptr<Ball> b);
	virtual void Collide(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);	
	virtual void Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);
	virtual void Bombcollision(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);
	virtual void Blackholecollision(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);
};

class Player : public Ball {

public:
	 shared_ptr<Player> findbiggest(vector<shared_ptr<Player>>& Oburs) {
		shared_ptr<Player> p = Oburs[0];
		for (auto a : Oburs) {
			if (p->mass < a->mass) {
				p = a;
			}
		}
		return p;
	}
	void destroydeadplayer(shared_ptr<Player> a);
	void checkmass(double mass);
	void drawobur(vector<shared_ptr<Player>>& Oburs);
	virtual shared_ptr<Player> Divide();
	void Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);
	Player(double x, double y, double m, olc::Pixel c, Game* g) :Ball(x, y, m, c, g) {}
    virtual void move(double base_vel, double fElapsedTime);

};
class objectwoutbonusorbad :public Ball {
	double remainingtime;
public:
	void countingtime(float fElapsedTime);
	void turn_tobomb();
	void turn_toblackhole();
	//random atamam
	objectwoutbonusorbad(double x, double y, double m, olc::Pixel c, Game* g) :Ball(x, y, m, c, g), remainingtime((double)(rand()%30)+20) {}	
	void Update(float fElapsedTime);
	void drawball();
};

class Bonus : public Ball {
	double& score;
public:
	void drawball();	
	Bonus(double x, double y, double m, olc::Pixel c, Game* g, double& s) :Ball(x, y, m, c, g), score(s) {}
	void Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);

};

class Bad : public Ball {

public:
	Bad(double x, double y, double m, olc::Pixel c, Game* g) :Ball(x, y, m, c, g) {}
	void drawball();
	void Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);

};
class Bomb :public Ball {
	
	double remainingtime;
public:
	void countingtime(float fElapsedTime);
	Bomb(double x, double y, double m, Game* g) :Ball(x, y, m, olc::RED, g), remainingtime(10)/*, radius(sqrt(mass)*5 ) */{}
	void Update(float fElapsedTime);
	void drawball();

	void checkifitisalive();


};
class Explosion :public Ball {

	double remainingtime;
public:
	void countingtime(float fElapsedTime);
	void drawball();

	void Collide(shared_ptr<Ball>a, double& grace, double& graceEvilperiod);
	Explosion(double x, double y, double m, Game* g) :Ball(x, y, m, olc::RED, g), remainingtime(0.5) {}
	void Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod);
	//to stop exploision
	void Update(float fElapsedTime);
	void checkifitisalive();

};
class BlackHole :public Ball {
	double remainingtime;
public:
	void countingtime(float fElapsedTime);
	BlackHole(double x, double y, double m, Game* g) :Ball(x, y, m, olc::VERY_DARK_YELLOW, g), remainingtime(30) {}
	void Collide(shared_ptr<Ball>a, double& grace, double& graceEvilperiod);
	void Collidewplayer(shared_ptr<Ball>a, double& grace, double& graceEvilperiod);
	
	//not to crash others and bomb
	void Blackholecollision(shared_ptr<Ball>a, double& grace, double& graceEvilperiod);
	void Bombcollision(shared_ptr<Ball>a, double& grace, double& graceEvilperiod);
	//to prevent others from pulling each other
	void gotoblackhole(double x, double y, float fElapsedTime);
	void drawball();
	void Update(float fElapsedTime);
	void checkifitisalive();
};

class Game : public olc::PixelGameEngine, public enable_shared_from_this<Game> {

	bool gameActive = true;
	double totalTime = 0;
	double score = 0;
	double gracePeriod = 0;
	double graceEvilPeriod = 0;
	int nofballs = 30;
	int nofbadguys = 10;
	int nofbonuses = 10;
	vector<shared_ptr<Ball>> otherthanplayers;
	vector<shared_ptr<Player>> Oburs;
public:

	bool OnUserCreate() //override
	{
		gameActive = true;
		score = 0;
		totalTime = 0;

		shared_ptr<Player> obur = make_shared <Player>(ScreenWidth() / 2, ScreenHeight() / 2, 100, olc::VERY_DARK_YELLOW, this);
		Oburs.push_back(obur);


		for (int i = 0; i < nofballs; ++i)
		{

			shared_ptr<Ball >x = make_shared<objectwoutbonusorbad>((double)rand() / RAND_MAX * ScreenWidth(), (double)rand() / RAND_MAX * ScreenHeight(), 70, olc::Pixel(rand() % 200, rand() % 200, rand() % 200), this);
			otherthanplayers.push_back(x);
		}
		for (int i = 0; i < nofbadguys; ++i)
		{
			shared_ptr<Bad >x = make_shared<Bad>((double)rand() / RAND_MAX * ScreenWidth(), (double)rand() / RAND_MAX * ScreenHeight(), 70, olc::Pixel(rand() % 200, rand() % 200, rand() % 200), this);
			otherthanplayers.push_back(x);
		}
		for (int i = 0; i < nofbonuses; ++i)
		{
			shared_ptr<Bonus >x = make_shared<Bonus>((double)rand() / RAND_MAX * ScreenWidth(), (double)rand() / RAND_MAX * ScreenHeight(), 70, olc::Pixel(rand() % 200, rand() % 200, rand() % 200), this, score);
			otherthanplayers.push_back(x);
		}
		return true;

	}

	double base_vel = 100;

	bool OnUserUpdate(float fElapsedTime) {

		if (GetKey(olc::SPACE).bPressed && gracePeriod == 0) {


			shared_ptr<Player>P = Oburs[0]->findbiggest(Oburs);
			Oburs.push_back(P->Divide());
			gracePeriod = 0.5;
		}

		for (auto a : Oburs) {
			a->move(base_vel, (double)fElapsedTime);
		}

		if (!gameActive) {
			Clear(olc::BLACK);
			DrawString(ScreenWidth() - 200, 5, "SCORE = " + to_string((int)score));
			DrawString(ScreenWidth() / 2 - 40, ScreenHeight() / 2 + 10, "");
			DrawString(ScreenWidth() / 2 - 40, ScreenHeight() / 2 + 30, "Y / N");

			if (GetKey(olc::Y).bPressed) {
				OnUserCreate();
				gameActive = true;
			}
			else if (GetKey(olc::N).bPressed) {
				bAtomActive = false;
			}
			return true;

		}


		if (Oburs.empty()) {
			gameActive = false;
			otherthanplayers.clear();
			return true;
		}

		totalTime += fElapsedTime;
		if (gracePeriod > 0) {
			gracePeriod -= fElapsedTime;
		}
		if (gracePeriod < 0) {
			gracePeriod = 0;
		}
		if (graceEvilPeriod > 0) {
			graceEvilPeriod -= fElapsedTime;
		}
		if (graceEvilPeriod < 0) {
			graceEvilPeriod = 0;
		}
		//updateforrevolution and grow
		for (auto a : otherthanplayers) {
			a->Update(fElapsedTime);
		}

		for (int i = 0; i < Oburs.size(); i++) {
			for (int j = i + 1; j < Oburs.size(); j++) {
				if (Oburs[i]->iscollided(Oburs[j])) {
					Oburs[i]->Collidewplayer(Oburs[j], gracePeriod, graceEvilPeriod);
				}
			}
		}

		for (auto a : otherthanplayers) {
			for (int i = 0; i < Oburs.size(); i++) {
				if (a->iscollided(Oburs[i])) {
					a->Collidewplayer(Oburs[i], gracePeriod, graceEvilPeriod);
				}
			}
		}
		for (int i = 0; i < otherthanplayers.size(); i++) {
			for (int j = i + 1; j < otherthanplayers.size(); j++) {
				if (otherthanplayers[i]->iscollided(otherthanplayers[j])) {
					otherthanplayers[i]->Collide(otherthanplayers[j], gracePeriod, graceEvilPeriod);
				}
			}
		}



		Clear(olc::BLACK);
		for (auto a : otherthanplayers) {
			a->drawball();
		}
		Oburs[0]->drawobur(Oburs);

		DrawCircle(GetMouseX(), GetMouseY(), 10, olc::VERY_DARK_RED);
		for (auto e : otherthanplayers) {
			e->checkifitisalive();
		}


		for (int i = Oburs.size() - 1; i >= 0; i--) {
			Oburs[i]->checkmass(7);
			Oburs[i]->destroydeadplayer(Oburs[i]);
		}


		Clear(olc::BLACK);
		DrawCircle(GetMouseX(), GetMouseY(), 10, olc::VERY_DARK_RED);
		for (auto a : Oburs) {
			a->drawobur(Oburs);
		}
		for (auto a : otherthanplayers) {
			a->drawball();

		}
		DrawString(ScreenWidth() - 200, 5, "SCORE= " + to_string((int)score));


		return true;
	}


public:
	void addobur(shared_ptr<Player> a);
	void removeobur(shared_ptr<Player>a);
	void usefunctforattributerefresh(shared_ptr<Ball>a);
	void pullingeverybody(double x, double y, float fElapsedTime);	
	void revolution(shared_ptr<Ball> a, shared_ptr<Ball> b);

};

void Ball::useitwhencreatingrandom(double Time) {

	xcor = ((double)rand() / RAND_MAX) * x->ScreenWidth();
	ycor = ((double)rand() / RAND_MAX) * x->ScreenHeight();
	mass = ((5.0 + (rand() % 2)) / 2.5) * (1 + Time);
	color = olc::Pixel(rand() % 200, rand() % 200, rand() % 200);
	isdead = false;

}
void Ball::drawball() {

	x->FillCircle(xcor, ycor, sqrt(mass), color);

}
void Ball::Update(float fElapsedTime) {

	massgrow((double)fElapsedTime * 5);
}
void Ball::checkifitisalive() {

	if (isdead) {
		x->usefunctforattributerefresh(shared_from_this());
	}

}

shared_ptr<Player> Ball::Divide() {
	return make_shared<Player>(0, 0, 0, 0, x);


}

bool Ball::iscollided(shared_ptr<Ball> b) {
	return sqrt(pow(xcor - b->xcor, 2) + pow(ycor - b->ycor, 2)) < sqrt(mass) + sqrt(b->mass);
}

void Ball::Collide(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	if (mass > a->mass) {
		mass += a->mass;
		a->isdead = true;
	}
	else {
		a->mass += mass;
		isdead = true;
	}
}
void Ball::Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {

	Ball::Collide(a, grace, graceEvilperiod);

}
void Ball::Bombcollision(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	isdead = true;
}
void Ball::Blackholecollision(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	isdead = true;
}

void Ball::gotoblackhole(double x, double y, float fElapsedTime) {
	double distancetoblackhole;
	distancetoblackhole = sqrt(pow(x - xcor, 2) + pow(y - ycor, 2));
	if (distancetoblackhole > 1) {

		xcor -= ((10 / pow(mass, 0.2)) * ((double)(xcor - x) / distancetoblackhole)) * fElapsedTime;
		ycor -= ((10 / pow(mass, 0.2)) * ((double)(ycor - y) / distancetoblackhole)) * fElapsedTime;


	}

}

void objectwoutbonusorbad::turn_tobomb() {
	x->revolution(shared_from_this(), make_shared<Bomb>(xcor, ycor, mass, x));
}
void objectwoutbonusorbad::turn_toblackhole() {
	x->revolution(shared_from_this(), make_shared<BlackHole>(xcor, ycor, mass, x));
}
void objectwoutbonusorbad::countingtime(float fElapsedTime) {
	remainingtime -= fElapsedTime;
	if (remainingtime < 0) {
		remainingtime = 0;
	}
}
void objectwoutbonusorbad::Update(float fElapsedTime) {
	Ball::Update(fElapsedTime);
	countingtime(fElapsedTime);
	if (remainingtime == 0) {
		if (rand() % 3) {
			turn_toblackhole();
		}
		else if (rand()%4) {
			turn_tobomb();
		}

	}

}

void objectwoutbonusorbad::drawball() {
	Ball::drawball();
}
void Player::checkmass(double Mass) {
	if (mass < Mass) {
		isdead = true;
	}
}

void Player::move(double base_vel, double fElapsedTime) {
	double distanceToMouse = sqrt(pow(x->GetMouseX() - xcor, 2) + pow(x->GetMouseY() - ycor, 2));

	if (distanceToMouse > 1) {

		xcor -= ((base_vel / pow(mass, 0.2)) * ((double)(xcor - x->GetMouseX()) / distanceToMouse)) * fElapsedTime;
		ycor -= ((base_vel / pow(mass, 0.2)) * ((double)(ycor - x->GetMouseY()) / distanceToMouse)) * fElapsedTime;

	}
}
void Player::destroydeadplayer(shared_ptr<Player> a) {
	if (isdead) {
		x->removeobur(a);
	}
}

void Player::Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	if (grace == 0) {
		Ball::Collide(a, grace, graceEvilperiod);
	}

}
//hocaya sor çalýþmýyor
shared_ptr<Player> Player::Divide() {
	mass *= 0.5;
	return make_shared<Player>(x->GetMouseX(), x->GetMouseY(), mass, color, x);


}
void Player::drawobur(vector<shared_ptr<Player>>& Oburs) {
	for (auto b : Oburs) {
		x->FillCircle(b->xcor, b->ycor, sqrt(b->mass), olc::BLUE);
	}
}
void Bad::Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	if (graceEvilperiod == 0) {
		a->massgrow(-mass);
		shared_from_this()->makeitbigger(a);
		x->addobur((shared_ptr<Player>)a->Divide());

		graceEvilperiod = 0.5;
		grace = 0.5;
	}

}

void Bad::drawball() {

	x->FillRect(xcor - sqrt(mass), ycor - sqrt(mass), 2 * sqrt(mass), 2 * sqrt(mass), color);

}


void Bonus::Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {

	score += mass;
	Ball::Collide(a, grace, graceEvilperiod);

}

void Bonus::drawball() {
	x->FillTriangle(xcor - sqrt(mass) - 1, ycor, xcor + sqrt(mass) + 1, ycor, xcor, ycor + sqrt(mass) + 2, color);

}
void Bomb::Update(float fElapsedTime) {
	countingtime(fElapsedTime);
	if (remainingtime == 0) {
		x->revolution(shared_from_this(), make_shared<Explosion>(xcor, ycor, mass*25, x));
	}

}

void Bomb::countingtime(float fElapsedTime) {


	remainingtime -= fElapsedTime;
	if (remainingtime < 0) {
		remainingtime = 0;
	}

}
void Bomb::drawball() {
	//kalan süreyi yazdýrmayý sor
	x->FillCircle(xcor, ycor, sqrt(mass), olc::VERY_DARK_CYAN);
	x->DrawCircle(xcor, ycor, sqrt(mass)*5, olc::VERY_DARK_RED);


}
void Bomb::checkifitisalive() {
	if (isdead) {
		//emin deðilsin hocaya sor
		//patlamaya dönüþtür
		auto a = make_shared<objectwoutbonusorbad>(10,10, 10, 10, x);
		x->usefunctforattributerefresh(a);
		x->revolution(shared_from_this(), a);

	}
}
void Explosion::countingtime(float fElapsedTime) {
	remainingtime -= fElapsedTime;
	if (remainingtime < 0) {
		remainingtime = 0;
	}
}
void Explosion::drawball() {
	x->FillCircle(xcor, ycor, sqrt(mass), olc::YELLOW);
	x->DrawString(xcor, ycor, "RUN", olc::BLACK, 2);
}
void Explosion::Collide(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	a->Bombcollision(shared_from_this(), grace, graceEvilperiod);

}
void Explosion::Collidewplayer(shared_ptr<Ball> a, double& grace, double& graceEvilperiod) {
	a->makeitdead();

}
void Explosion::Update(float fElapsedTime) {
	Explosion::countingtime(fElapsedTime);
	if (remainingtime == 0) {
		isdead = true;
	}
}
//bu fonksiyona hocayla bak
void Explosion::checkifitisalive() {
	if (isdead) {
		
		auto n = make_shared<objectwoutbonusorbad>(10, 10, 10, olc::RED, x);
		x->usefunctforattributerefresh(n);
		x->revolution(shared_from_this(), n);
	}
}
void BlackHole::drawball() {
	x->FillCircle(xcor, ycor, sqrt(mass), color);
	x->DrawCircle(xcor, ycor, sqrt(mass), olc::RED);
	x->DrawString(xcor, ycor,"BLACKHOLE", 1);

}
void BlackHole::countingtime(float fElapsedTime) {
	remainingtime -= fElapsedTime;
	if (remainingtime < 0) {
		remainingtime = 0;
	}
}
void BlackHole::Update(float fElapsedTime) {
	x->pullingeverybody(xcor, ycor, fElapsedTime);
	BlackHole::countingtime(fElapsedTime);
	if (remainingtime == 0) {
		shared_from_this()->makeitdead();

	}
}
//hocaya sor explosion çalýþýyo bu çalýþmýyo
void BlackHole::checkifitisalive() {
	if (isdead) {
		
		auto a = make_shared<objectwoutbonusorbad>(0, 0, 0, 0, x);
		x->usefunctforattributerefresh(a);
		x->revolution(shared_from_this(), a);

	}
}
void BlackHole::Collide(shared_ptr<Ball>a, double& grace, double& graceEvilperiod) {
	a->Blackholecollision(shared_from_this(), grace, graceEvilperiod);
}
void BlackHole::Collidewplayer(shared_ptr<Ball>a, double& grace, double& graceEvilperiod) {
	a->makeitdead();
}
void BlackHole::Bombcollision(shared_ptr<Ball>a, double& grace, double& graceEvilperiod) {}
void BlackHole::Blackholecollision(shared_ptr<Ball>a, double& grace, double& graceEvilperiod) {}
void BlackHole::gotoblackhole(double x, double y, float fElapsedTime) {}
void Game::revolution(shared_ptr<Ball> a, shared_ptr<Ball> b) {

	replace(otherthanplayers.begin(), otherthanplayers.end(), a, b);

}
void Game::addobur(shared_ptr<Player> a) {
	Oburs.push_back(a);
}
void Game::removeobur(shared_ptr<Player> a) {

	Oburs.erase(find(Oburs.begin(), Oburs.end(), a));
}

void Game::usefunctforattributerefresh(shared_ptr<Ball>a) {

	a->useitwhencreatingrandom(totalTime);

}

void Game::pullingeverybody(double x, double y, float fElapsedTime) {
	for (auto a : otherthanplayers) {
		a->gotoblackhole(x, y, fElapsedTime);
	}
	for (auto b : Oburs) {
		b->gotoblackhole(x,y,fElapsedTime);
	}
	
}

int main() {

	Game demo;

	if (demo.Construct(1440, 1000, 1, 1)) //game screen size is 960 x 720, each game pixel is one screen pixel. Reduce this if your screen resolution is lower
		demo.Start();

	return 0;

}



