//---------------------------------------------------------------------------

#ifndef osInfoH
#define osInfoH
//---------------------------------------------------------------------------

enum SISTEMAS_WIN{
	WIN_DESCONHECIDO,
	WIN_VISTA,
	WIN_SERVER,
	WIN_SERVER_2008,
	//WIN_SERVER_2008_R2,
	WIN_7,
	WIN_SERVER_2003,
	//WIN_SERVER_2003_R2,
	WIN_XP,
	//WIN_XP_64,
	WIN_2000
};

enum TIPO_SISTEMA{
	SYS_32,
	SYS_64
};

typedef struct OS_INFO{
	int OS;
	TIPO_SISTEMA arquitetura;
	int versao;
	OSVERSIONINFOEX info;
}OS_INFO;

OS_INFO GetOS();
#endif
