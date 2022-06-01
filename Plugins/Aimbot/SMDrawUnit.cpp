//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SMDrawUnit.h"
#include "Threads.h"
#include "BotUnit.h"
#include "Sharing.h"
#include <math.h>
#pragma package(smart_init)

#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"

GdiplusStartupInput gdiplusStartupInput;
ULONG_PTR           gdiplusToken;
//---------------------------------------------------------------------------

//   Important: Methods and properties of objects in VCL can only be
//   used in a method called using Synchronize, for example:
//
//      Synchronize(&UpdateCaption);
//
//   where UpdateCaption could look like:
//
//      void __fastcall TSMDraw::UpdateCaption()
//      {
//        Form1->Caption = "Updated in a thread";
//      }
//---------------------------------------------------------------------------

__fastcall TSMDraw::TSMDraw(bool CreateSuspended)
	: TThread(CreateSuspended)
{
     GDIStarted = true;
	 Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
}

TPoint __fastcall GetCameraPos()
{
	DWORD addressX = 0;
	DWORD addressY = 0;
	int cameraX = 0;
	int cameraY = 0;

	TPoint camera(0,0);

	CMReadProcessMemory(MatrizInfo->TargetId, MatrizInfo->TargetBase, (PVOID)(Pacotes[1].Ponteiro), &addressX, 4);
	CMReadProcessMemory(MatrizInfo->TargetId, MatrizInfo->TargetBase, (PVOID)(addressX+Pacotes[1].Offset), &cameraX, Pacotes[1].Size);
	CMReadProcessMemory(MatrizInfo->TargetId, MatrizInfo->TargetBase, (PVOID)(Pacotes[2].Ponteiro), &addressY, 4);
	CMReadProcessMemory(MatrizInfo->TargetId, MatrizInfo->TargetBase, (PVOID)(addressY+Pacotes[2].Offset), &cameraY, Pacotes[2].Size);
	camera.x = cameraX-400;
	camera.y = cameraY-240;
	return camera;
}

double __fastcall IntToRadian(int valor)
{
   return valor*pi/180;
}

typedef struct _DrawPoint
{
	 float time;
	 bool drawed;
} TDrawPoint;

typedef struct _CM_RGB
{
	 char r;
	 char g;
	 char b;
	 char x;
} TCM_RGB, *PCM_RGB;

Gdiplus::Color ToGdiColor(DWORD valor)
{
		TCM_RGB result = *(PCM_RGB)(&valor);
		result.x = 0xFF;
		return Gdiplus::Color(result.x , result.r, result.g, result.b);
}

void __fastcall FillAimPoints()
{
		//if(!ShotMatrix )
		//	 return;

		if( ShotMatrix->pausado )
			 return;


		HDC dc = 0;
		dc = CreateDC("DISPLAY", "", "", NULL);
		if(!dc) return;

	__try
	{
				__try
				{
						TPoint cam = GetCameraPos();
						int cameraX = cam.x;
						int cameraY = cam.y;
						int espessura = EspessuraAim;

						//Cria PENS com suas cores
						Gdiplus::Pen whitePen(ToGdiColor(0xFFFFFF), espessura);
						Gdiplus::Pen whitePen2(ToGdiColor(0xFFFFFF));
						Gdiplus::Pen customPen1(ToGdiColor(ShotMatrix->corLinha1), espessura);
						Gdiplus::Pen customPen2(ToGdiColor(ShotMatrix->corLinha2), espessura);
						Gdiplus::Pen redPen(ToGdiColor(clBlue), 1);
						Gdiplus::Pen bluePen(ToGdiColor(clRed), 1);

						const int raio = 36;
						Gdiplus::Graphics grap(dc);

						int tx = ShotMatrix->bot.x - cameraX;
						int ty = ShotMatrix->bot.y - cameraY;

						int posx = raio*cos(IntToRadian(ShotMatrix->estadoBot.angle));
						int posy = raio*sin(IntToRadian(ShotMatrix->estadoBot.angle));

						grap.DrawEllipse(&redPen, tx - raio, ty - raio, 2*raio, 2*raio);
						grap.DrawLine(&whitePen2, tx, ty, tx+posx, ty-posy);

						if((int)ShotMatrix->modoMira < 2 || (int)ShotMatrix->modoMira > 3)
						{
								tx = ShotMatrix->target.x - cameraX;
								ty = ShotMatrix->target.y - cameraY;

								grap.DrawEllipse(&bluePen, tx - raio, ty - raio, 2*raio, 2*raio);
								grap.DrawLine(&whitePen2, tx, ty - raio, tx, ty + raio);
								grap.DrawLine(&whitePen2, tx-raio, ty, tx+raio, ty);
						}

						if(ShotMatrix->GameInterface == 1)
						{
							 grap.DrawLine(&whitePen, 389+ShotMatrix->botState.power, 565, 389+ShotMatrix->botState.power, 585);
						}
						else
						{
							 grap.DrawLine(&whitePen, 241+ShotMatrix->botState.power, 565, 241+ShotMatrix->botState.power, 585);
						}
						//------------------------------------

						double Alfa,Beta,Gama,Delta; //Variaveis de controle de funcções aritméticas - sen, cos tg
						double WindSpeedX = 0;
						double WindSpeedY = 0;
						double ShotSpeedX = 0;
						double ShotSpeedY = 0;

						Alfa = cos(IntToRadian(ShotMatrix->botState.angle));//+BotInfos->XEffect;
						Beta = sin(IntToRadian(ShotMatrix->botState.angle));//+BotInfos->YEffect;
						Gama = cos(IntToRadian(ShotMatrix->windState.angle));
						Delta = sin(IntToRadian(ShotMatrix->windState.angle));

						//WindSpeedX = (BotInfos->WindSpeed * Gama * (BotInfos->WindEffect.x) );
						//WindSpeedY = (BotInfos->WindSpeed * Delta * (BotInfos->WindEffect.y) ) - BotInfos->Gravity;

						WindSpeedX = (ShotMatrix->windState.power * Gama * ShotMatrix->BotEffect.WindEffect);
						WindSpeedY = (ShotMatrix->windState.power * Delta * ShotMatrix->BotEffect.WindEffect ) - ShotMatrix->BotEffect.Gravity;

						double Menor = 9999;
						int Melhor = 0;
						TProjetil Projetil;

					 int PxCount = 0;

					 ShotSpeedX = ShotMatrix->botState.power * Alfa;
					 ShotSpeedY = ShotMatrix->botState.power * Beta;

					 Projetil.x = ShotMatrix->bot.x;
					 Projetil.y = ShotMatrix->bot.y;
					 Projetil.SpeedX = ShotSpeedX;  //ShotSpeedX;
					 Projetil.SpeedY = ShotSpeedY;  //ShotSpeedY;

					 //Sleep(1);
					 bool flag = false;
					 
					 TPoint ultimo;
					 ultimo.x = Projetil.x - cameraX;
					 ultimo.y = Projetil.y - cameraY;

					 BOOL ss = false;
					 double TotalTime = 0;

					 BOOL drawed = false;
					 BOOL breaked = false;

					 //char sval[20];
					 //itoa(BotInfos->SSTime.t1, sval, 10);

					 char sval[101];

					float ltcrx = Projetil.x;
					float ltcry = Projetil.y;
					float ltcrx2 = Projetil.x;
					float ltcry2 = Projetil.y;

					std::vector<TPointFloat> L1;
					std::vector<TPointFloat> L2;
					std::vector<TPointFloat> Traj; //SpecialTrajectory

					 BOOL L1D = false, L1F = false, L1B = false;
					 BOOL L2D = false, L2F = false, L2B = false;

					 if(Projetil.SpeedX != 0 || Projetil.SpeedY != 0)
					 while(Projetil.x < 1800 && Projetil.x > 0 && Projetil.y > -5000 && Projetil.y < 2000)
					 {
								Projetil.x += Projetil.SpeedX * Tempo;
								Projetil.y -= Projetil.SpeedY * Tempo; //((Projetil.SpeedY)*Tempo - (BotInfos->Gravity*Tempo*Tempo*0.5));
								Projetil.SpeedX += WindSpeedX * Tempo;
								Projetil.SpeedY += WindSpeedY * Tempo;
								TotalTime += Tempo;

								//Calculo cara trico
								float T;
								float t;
								float tca;
								float tca2;
								float tcr;
								float tcrlx;
								float tcrly;
								
								float tcrlx2;
								float tcrly2;

								float tcrx;
								float tcrx2;
								float tcry;
								float tcry2;

								if( ShotMatrix->Mobile == 3 )
								{
                    T = pontos[0].time;
										if(T == 0) T = 120;
										t = (TotalTime/0.05f);
										
										//if(BotInfos->extra2 == 0)
										//{
										//	tca2 = IntToRadian( t*(360/T) + BotInfos->extra1 - 180 );
										//  tca = IntToRadian( t*(360/T) + BotInfos->extra1 );
										//}
										//else
										//	tca = IntToRadian( t*(360/T) ) + BotInfos->extra1;
										tca = IntToRadian( t*(360/T) );
										tca2 = IntToRadian( t*(360/T)-180 );
											
										tcr = 43;
										tcrlx = tcr * cos(tca);
										tcrly = tcr * sin(tca);

										/*if(BotInfos->extra2 != 0)
										{
											tcrlx2 = tcr * cos(tca - 180 + BotInfos->extra1);
											tcrly2 = tcr * sin(tca - 180 + BotInfos->extra1);
										}
										else
										{
											tcrlx2 = tcr * cos(tca2);
											tcrly2 = tcr * sin(tca2);
										} */
										tcrlx2 = tcr * cos(tca2);
										tcrly2 = tcr * sin(tca2);

										tcrx = Projetil.x + tcrlx;
										tcrx2 = Projetil.x + tcrlx2;
										tcry = Projetil.y - tcrly;
										tcry2 = Projetil.y - tcrly2;

										TPointFloat pt;
										pt.x = tcrx;
										pt.y = tcry;
										L1.push_back( pt );
										pt.x = tcrx2;
										pt.y = tcry2;
										L2.push_back( pt );
										pt.x = Projetil.x - cameraX;
										pt.y = Projetil.y - cameraY;
										Traj.push_back( pt );
								}

								//**********************************************
								//Desenha Linha 1 da trajetoria especial
								if( (tcrx > (cameraX-20)) && (tcrx < (cameraX+820)) && (tcry > (cameraY-20)) && (tcry < (cameraY+525))  )
								{
										L1D = true;
										if(L1B)
										{
												L1F = false;
												L1B = false;
										}

										if(!L1F)
										{
											 L1F = true;
											 ltcrx = tcrx - cameraX;
											 ltcry = tcry - cameraY;
										}
										else  
										{
												if(ShotMatrix->ShowTrajectory && ShotMatrix->Mobile == 3)
												{
														tcrx -= cameraX;
														tcry -= cameraY;
														grap.DrawLine(&customPen2,(int) ltcrx, (int)ltcry, (int)tcrx, (int)tcry);
														ltcrx = tcrx;
														ltcry = tcry;
												}
										}
								 }
								 else
								 {
											if(L1D)
												L1B = true;
								 }
									//**********************************************


									//**********************************************
									//Desenha Linha 2 da trajetoria especial
								 if( (tcrx2 > (cameraX-20)) && (tcrx2 < (cameraX+820)) && (tcry2 > (cameraY-20)) && (tcry2 < (cameraY+525))  )
								 {
											L2D = true;
											if(L1B)
											{
													L2F = false;
													L2B = false;
											}

											if(!L2F)
											{
												 L2F = true;
												 ltcrx2 = tcrx2 - cameraX;
												 ltcry2 = tcry2 - cameraY;
											}
											else
											{
													if(ShotMatrix->ShowTrajectory && ShotMatrix->Mobile == 3)
													{
															tcrx2 -= cameraX;
															tcry2 -= cameraY;
															grap.DrawLine(&customPen2,(int) ltcrx2, (int)ltcry2, (int)tcrx2, (int)tcry2);
															ltcrx2 = tcrx2;
															ltcry2 = tcry2;
													}
											}
								 }
								 else
								 {
										if(L2D)
											L2B = true;
								 }

								 //**********************************************
								 //Desenha Linha da trajetória normal
								 if( (Projetil.x > (cameraX-20)) && (Projetil.x < (cameraX+820)) && (Projetil.y > (cameraY-20)) && (Projetil.y < (cameraY+525))  )
								 {
										drawed = true;

										//evita q a trajetoria "quebre" na tela
										if(breaked)
										{
												flag = false;
												breaked = false;
										}

										if(!flag)
										{
											 flag = true;
											 ultimo.x = Projetil.x - cameraX;
											 ultimo.y = Projetil.y - cameraY;
										}
										else  
										{
												TPoint atual;
												atual.x = Projetil.x - cameraX;
												atual.y = Projetil.y - cameraY;

												for(int i = 0; i < 5; i++)
												{
													 if(pontos[i].time != 0 && !pontos[i].drawed && TotalTime >= pontos[i].time)
													 {
															int r = espessura*3;
															grap.DrawEllipse(&whitePen, ultimo.x-r, ultimo.y-r, r*2, r*2);
                              pontos[i].drawed = true;
													 }
												}
												
												if(ShotMatrix->ChangeColor && pontos[0].time > 0 && TotalTime >= pontos[0].time)
													 grap.DrawLine(&customPen2,(int) ultimo.x, (int)ultimo.y, (int)atual.x, (int)atual.y);
												else
												   grap.DrawLine(&customPen1,(int) ultimo.x, (int)ultimo.y, (int)atual.x, (int)atual.y);

												ultimo = atual;
										}
								 }
								 else
								 {
										if(drawed)
										  breaked = true;
								 }
                 //**********************************************
						}

						//Calcula pontos do trico
						BOOL printed = false;
						int LastJ = 1;
						for(int i = 1; i < L1.size(); i++)
						{
								float pxi1 = (L1[i-1].x < L1[i].x) ? L1[i-1].x : L1[i].x;
								float pxf1 = (L1[i-1].x < L1[i].x) ? L1[i].x : L1[i-1].x;
								float pyi1 = (L1[i-1].y < L1[i].y) ? L1[i-1].y : L1[i].y;
								float pyf1 = (L1[i-1].y < L1[i].y) ? L1[i].y : L1[i-1].y;

								if(!printed)
								{
										for(int j = LastJ; j < L2.size(); j++)
										{
												float pxi2 = (L2[j-1].x < L2[j].x) ? L2[j-1].x : L2[j].x;
												float pxf2 = (L2[j-1].x < L2[j].x) ? L2[j].x : L2[j-1].x;
												float pyi2 = (L2[j-1].y < L2[j].y) ? L2[j-1].y : L2[j].y;
												float pyf2 = (L2[j-1].y < L2[j].y) ? L2[j].y : L2[j-1].y;

												float tx = 0,ty = 0;
												if( (int)pxi1 < (int)pxi2 )
												{
														if( (int)pxi2 <= (int)pxf1 )
														{
																tx = pxf2 - cameraX;
																if( (int)pyi1 > (int)pyi2 )
																{
																	 if((int)pyi1 <= (int)pyf2)
																		 ty = pyi1 - cameraY;
																}
																else
																{
																	 if((int)pyi2 <= (int)pyf1)
																		 ty = pyi2 - cameraY;
																}
														}
												}
												else
												{
														if( (int)pxi1 < (int)pxf2 )
														{
																tx = pxi1 - cameraX;
																if( (int)pyi1 > (int)pyi2 )
																{
																	 if((int)pyi1 <= (int)pyf2)
																		 ty = pyi1 - cameraY;
																}
																else
																{
																	 if((int)pyi2 <= (int)pyf1)
																		 ty = pyi2 - cameraY;
																}
														}
												}
												if(tx > 0 && ty > 0 && !printed)
												{
														 float r = (espessura*4);
														 int k = (i+j)/2;
														 grap.DrawEllipse(&whitePen, Traj[k].x-r, Traj[k].y-r, 2*r, 2*r);
														 printed = true;
														 LastJ = j+3;
														 break;
												}
										}
								}
								else
                  printed = false;

						}
			
			}
			__except(1)
			{
					return;
			}
	}
	__finally                         
	{
			DeleteDC(dc);
	}

}

//---------------------------------------------------------------------------
void __fastcall TSMDraw::Execute()
{
   if(!GDIStarted)
	 {
	    GDIStarted = true;
			Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
	 }

		while(!Terminated)
		{
			 //FillAimPoints();
			 //Sleep(5);
				Sleep(5);
				if(!ShotMatrix)
				   continue;

				if( !ShotMatrix->pausado )
				{

							HDC dc = 0;
							dc = CreateDC("DISPLAY", "", "", NULL);
							if(!dc) continue;

							__try
							{
										__try
										{
													TPoint cam = GetCameraPos();
													int cameraX = cam.x;
													int cameraY = cam.y;
													int espessura = EspessuraAim;

													//Cria PENS com suas cores
													Gdiplus::Pen whitePen(ToGdiColor(0xFFFFFF), espessura);
													Gdiplus::Pen whitePen2(ToGdiColor(0xFFFFFF));
													Gdiplus::Pen customPen1(ToGdiColor(ShotMatrix->LineColor1), espessura);
													Gdiplus::Pen customPen2(ToGdiColor(ShotMatrix->LineColor2), espessura);
													Gdiplus::Pen redPen(ToGdiColor(clBlue), 1);
													Gdiplus::Pen bluePen(ToGdiColor(clRed), 1);

													const int raio = 36;
													Gdiplus::Graphics grap(dc);

													int tx = ShotMatrix->bot.x - cameraX;
													int ty = ShotMatrix->bot.y - cameraY;

													int posx = raio*cos(IntToRadian(ShotMatrix->botState.angle));
													int posy = raio*sin(IntToRadian(ShotMatrix->botState.angle));

													grap.DrawEllipse(&redPen, tx - raio, ty - raio, 2*raio, 2*raio);
													grap.DrawLine(&whitePen2, tx, ty, tx+posx, ty-posy);

													if((int)ShotMatrix->AimMode < 2 || (int)ShotMatrix->AimMode > 3)
													{
															tx = ShotMatrix->target.x - cameraX;
															ty = ShotMatrix->target.y - cameraY;

															grap.DrawEllipse(&bluePen, tx - raio, ty - raio, 2*raio, 2*raio);
															grap.DrawLine(&whitePen2, tx, ty - raio, tx, ty + raio);
															grap.DrawLine(&whitePen2, tx-raio, ty, tx+raio, ty);
													}

													if(ShotMatrix->GameInterface == 1)
													{
														 grap.DrawLine(&whitePen, 389+ShotMatrix->botState.power, 565, 389+ShotMatrix->botState.power, 585);
													}
													else
													{
														 grap.DrawLine(&whitePen, 241+ShotMatrix->botState.power, 565, 241+ShotMatrix->botState.power, 585);
													}
													//------------------------------------
													Sleep(1);

													double Alfa,Beta,Gama,Delta; //Variaveis de controle de funcções aritméticas - sen, cos tg
													double WindSpeedX = 0;
													double WindSpeedY = 0;
													double ShotSpeedX = 0;
													double ShotSpeedY = 0;

													Alfa = cos(IntToRadian(ShotMatrix->botState.angle));//+BotInfos->XEffect;
													Beta = sin(IntToRadian(ShotMatrix->botState.angle));//+BotInfos->YEffect;
													Gama = cos(IntToRadian(ShotMatrix->windState.angle));
													Delta = sin(IntToRadian(ShotMatrix->windState.angle));

														//WindSpeedX = (BotInfos->WindSpeed * Gama * (BotInfos->WindEffect.x) );
														//WindSpeedY = (BotInfos->WindSpeed * Delta * (BotInfos->WindEffect.y) ) - BotInfos->Gravity;

													WindSpeedX = (ShotMatrix->windState.power * Gama * ShotMatrix->BotEffect.WindEffect);
													WindSpeedY = (ShotMatrix->windState.power * Delta * ShotMatrix->BotEffect.WindEffect ) - ShotMatrix->BotEffect.Gravity;

													double Menor = 9999;
													int Melhor = 0;
													TProjetil Projetil;

													int PxCount = 0;

													ShotSpeedX = ShotMatrix->botState.power * Alfa;
													ShotSpeedY = ShotMatrix->botState.power * Beta;

													Projetil.x = ShotMatrix->bot.x;
													Projetil.y = ShotMatrix->bot.y;
													Projetil.SpeedX = ShotSpeedX;  //ShotSpeedX;
													Projetil.SpeedY = ShotSpeedY;  //ShotSpeedY;

													 //Sleep(1);
													bool flag = false;
					 
													TPoint ultimo;
													ultimo.x = Projetil.x - cameraX;
													ultimo.y = Projetil.y - cameraY;

													BOOL ss = false;
													double TotalTime = 0;

													BOOL drawed = false;
													BOOL breaked = false;

													 //char sval[20];
													 //itoa(BotInfos->SSTime.t1, sval, 10);

													char sval[101];
													 //sprintf(sval, "%G", BotInfos->SSTime.t1);
													 //TextOut(dc, 10, 10, sval, 20);
					 
													TDrawPoint pontos[5] = {
																										{ShotMatrix->SSTime.t1 , false},
																										{ShotMatrix->SSTime.t2 , false},
																										{ShotMatrix->SSTime.t3 , false},
																										{ShotMatrix->SSTime.t4 , false},
																										{ShotMatrix->SSTime.t5 , false}
																									};

													 //sprintf(sval, "%G - %d", pontos[0].time, BotInfos->mobile);
													 //TextOut(dc, 600, 10, sval, 20);

													float ltcrx = Projetil.x;
													float ltcry = Projetil.y;
													float ltcrx2 = Projetil.x;
													float ltcry2 = Projetil.y;

													std::vector<TPointFloat> L1;
													std::vector<TPointFloat> L2;
													std::vector<TPointFloat> Traj; //SpecialTrajectory

													BOOL L1D = false, L1F = false, L1B = false;
													BOOL L2D = false, L2F = false, L2B = false;

													Sleep(1);

													if(Projetil.SpeedX != 0 || Projetil.SpeedY != 0)
													while(Projetil.x < 1800 && Projetil.x > 0 && Projetil.y > -5000 && Projetil.y < 2000)
													{
																Projetil.x += Projetil.SpeedX * Tempo;
																Projetil.y -= Projetil.SpeedY * Tempo; //((Projetil.SpeedY)*Tempo - (BotInfos->Gravity*Tempo*Tempo*0.5));
																Projetil.SpeedX += WindSpeedX * Tempo;
																Projetil.SpeedY += WindSpeedY * Tempo;
																TotalTime += Tempo;

																//Calculo cara trico
																float T;
																float t;
																float tca;
																float tca2;
																float tcr;
																float tcrlx;
																float tcrly;
								
																float tcrlx2;
																float tcrly2;

																float tcrx;
																float tcrx2;
																float tcry;
																float tcry2;

																if( ShotMatrix->Mobile == 3 )
																{
																		T = pontos[0].time;
																		if(T == 0) T = 120;
																		t = (TotalTime/0.05f);
										
																		//if(BotInfos->extra2 == 0)
																		//{
																		//	tca2 = IntToRadian( t*(360/T) + BotInfos->extra1 - 180 );
																		//  tca = IntToRadian( t*(360/T) + BotInfos->extra1 );
																		//}
																		//else
																		//	tca = IntToRadian( t*(360/T) ) + BotInfos->extra1;
																		tca = IntToRadian( t*(360/T) );
																		tca2 = IntToRadian( t*(360/T)-180 );
											
																		tcr = 43;
																		tcrlx = tcr * cos(tca);
																		tcrly = tcr * sin(tca);

																		/*if(BotInfos->extra2 != 0)
																		{
																			tcrlx2 = tcr * cos(tca - 180 + BotInfos->extra1);
																			tcrly2 = tcr * sin(tca - 180 + BotInfos->extra1);
																		}
																		else
																		{
																			tcrlx2 = tcr * cos(tca2);
																			tcrly2 = tcr * sin(tca2);
																		} */
																		tcrlx2 = tcr * cos(tca2);
																		tcrly2 = tcr * sin(tca2);

																		tcrx = Projetil.x + tcrlx;
																		tcrx2 = Projetil.x + tcrlx2;
																		tcry = Projetil.y - tcrly;
																		tcry2 = Projetil.y - tcrly2;

																		TPointFloat pt;
																		pt.x = tcrx;
																		pt.y = tcry;
																		L1.push_back( pt );
																		pt.x = tcrx2;
																		pt.y = tcry2;
																		L2.push_back( pt );
																		pt.x = Projetil.x - cameraX;
																		pt.y = Projetil.y - cameraY;
																		Traj.push_back( pt );
																}

																//**********************************************
																//Desenha Linha 1 da trajetoria especial
																if( (tcrx > (cameraX-20)) && (tcrx < (cameraX+820)) && (tcry > (cameraY-20)) && (tcry < (cameraY+525))  )
																{
																			L1D = true;
																			if(L1B)
																			{
																					L1F = false;
																					L1B = false;
																			}

																			if(!L1F)
																			{
																				 L1F = true;
																				 ltcrx = tcrx - cameraX;
																				 ltcry = tcry - cameraY;
																			}
																			else  
																			{
																					if(ShotMatrix->ShowTrajectory && ShotMatrix->Mobile == 3)
																					{
																							tcrx -= cameraX;
																							tcry -= cameraY;
																							grap.DrawLine(&customPen2,(int) ltcrx, (int)ltcry, (int)tcrx, (int)tcry);
																							ltcrx = tcrx;
																							ltcry = tcry;
																					}
																			}
																}
																else
																{
																			if(L1D)
																				L1B = true;
																}
																//**********************************************


																//**********************************************
																//Desenha Linha 2 da trajetoria especial
																if( (tcrx2 > (cameraX-20)) && (tcrx2 < (cameraX+820)) && (tcry2 > (cameraY-20)) && (tcry2 < (cameraY+525))  )
																{
																			L2D = true;
																			if(L1B)
																			{
																					L2F = false;
																					L2B = false;
																			}

																			if(!L2F)
																			{
																				 L2F = true;
																				 ltcrx2 = tcrx2 - cameraX;
																				 ltcry2 = tcry2 - cameraY;
																			}
																			else
																			{
																					if(ShotMatrix->ShowTrajectory && ShotMatrix->Mobile == 3)
																					{
																							tcrx2 -= cameraX;
																							tcry2 -= cameraY;
																							grap.DrawLine(&customPen2,(int) ltcrx2, (int)ltcry2, (int)tcrx2, (int)tcry2);
																							ltcrx2 = tcrx2;
																							ltcry2 = tcry2;
																					}
																			}
																}
																else
																{
																		if(L2D)
																			L2B = true;
																}

																//**********************************************
																//Desenha Linha da trajetória normal
																if( (Projetil.x > (cameraX-20)) && (Projetil.x < (cameraX+820)) && (Projetil.y > (cameraY-20)) && (Projetil.y < (cameraY+525))  )
																{
																		drawed = true;

																		//evita q a trajetoria "quebre" na tela
																		if(breaked)
																		{
																				flag = false;
																				breaked = false;
																		}

																		if(!flag)
																		{
																			 flag = true;
																			 ultimo.x = Projetil.x - cameraX;
																			 ultimo.y = Projetil.y - cameraY;
																		}
																		else  
																		{
																				TPoint atual;
																				atual.x = Projetil.x - cameraX;
																				atual.y = Projetil.y - cameraY;

																				for(int i = 0; i < 5; i++)
																				{
																					 if(pontos[i].time != 0 && !pontos[i].drawed && TotalTime >= pontos[i].time)
																					 {
																							int r = espessura*3;
																							grap.DrawEllipse(&whitePen, ultimo.x-r, ultimo.y-r, r*2, r*2);
																							pontos[i].drawed = true;
																					 }
																				}
												
																				if(ShotMatrix->ChangeColor && pontos[0].time > 0 && TotalTime >= pontos[0].time)
																					 grap.DrawLine(&customPen2,(int) ultimo.x, (int)ultimo.y, (int)atual.x, (int)atual.y);
																				else
																					 grap.DrawLine(&customPen1,(int) ultimo.x, (int)ultimo.y, (int)atual.x, (int)atual.y);

																				ultimo = atual;
																		}
																}
																else
																{
																		if(drawed)
																			breaked = true;
																}
																 //**********************************************
													 }

														//Calcula pontos do trico
													 BOOL printed = false;
													 int LastJ = 1;
													 for(int i = 1; i < L1.size(); i++)
													 {
																float pxi1 = (L1[i-1].x < L1[i].x) ? L1[i-1].x : L1[i].x;
																float pxf1 = (L1[i-1].x < L1[i].x) ? L1[i].x : L1[i-1].x;
																float pyi1 = (L1[i-1].y < L1[i].y) ? L1[i-1].y : L1[i].y;
																float pyf1 = (L1[i-1].y < L1[i].y) ? L1[i].y : L1[i-1].y;

																if(!printed)
																{
																		for(int j = LastJ; j < L2.size(); j++)
																		{
																				float pxi2 = (L2[j-1].x < L2[j].x) ? L2[j-1].x : L2[j].x;
																				float pxf2 = (L2[j-1].x < L2[j].x) ? L2[j].x : L2[j-1].x;
																				float pyi2 = (L2[j-1].y < L2[j].y) ? L2[j-1].y : L2[j].y;
																				float pyf2 = (L2[j-1].y < L2[j].y) ? L2[j].y : L2[j-1].y;

																				float tx = 0,ty = 0;
																				if( (int)pxi1 < (int)pxi2 )
																				{
																						if( (int)pxi2 <= (int)pxf1 )
																						{
																								tx = pxf2 - cameraX;
																								if( (int)pyi1 > (int)pyi2 )
																								{
																									 if((int)pyi1 <= (int)pyf2)
																										 ty = pyi1 - cameraY;
																								}
																								else
																								{
																									 if((int)pyi2 <= (int)pyf1)
																										 ty = pyi2 - cameraY;
																								}
																						}
																				}
																				else
																				{
																						if( (int)pxi1 < (int)pxf2 )
																						{
																								tx = pxi1 - cameraX;
																								if( (int)pyi1 > (int)pyi2 )
																								{
																									 if((int)pyi1 <= (int)pyf2)
																										 ty = pyi1 - cameraY;
																								}
																								else
																								{
																									 if((int)pyi2 <= (int)pyf1)
																										 ty = pyi2 - cameraY;
																								}
																						}
																				}
																				if(tx > 0 && ty > 0 && !printed)
																				{
																						 float r = (espessura*4);
																						 int k = (i+j)/2;
																						 grap.DrawEllipse(&whitePen, Traj[k].x-r, Traj[k].y-r, 2*r, 2*r);
																						 printed = true;
																						 LastJ = j+3;
																						 break;
																				}
																		}
														}
														else
																printed = false;

												}
			
									}
									__except(1)
									{
											return;
									}
							}
							__finally                         
							{
									DeleteDC(dc);
							}
					}

					
		}

		if(GDIStarted)
		{
			Gdiplus::GdiplusShutdown(gdiplusToken);
			GDIStarted = false;
		}
	//---- Place thread code here ----
}
//---------------------------------------------------------------------------
