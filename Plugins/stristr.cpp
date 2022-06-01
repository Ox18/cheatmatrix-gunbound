// If this code works, it was written by Ralph Walden. If it doesn't work, I don't know who wrote it.

#include "stristr.h"

char* __fastcall stristrA(const char* pszMain, const char* pszSub)
{
    pszMain;    // compiler thinks these are unreferenced because
    pszSub;     // they are in ecx and edx registers

    char* pszTmp1;
    char* pszTmp2;
    char  lowerch, upperch;

// We keep the first character of pszSub in lowerch and upperch (lower and
// upper case). First we loop trying to find a match for this character. Once
// we have found a match, we start with the second character of both pszMain
// and pszSub and walk through both strings doing a CharLower on both
// characters before comparing. If we make it all the way through pszSub with
// matches, then we bail with a pointer to the string's location in pszMain.

    _asm {
        mov esi, ecx    // pszMain
        mov edi, edx    // pszSub

        // Check for NULL pointers

        test ecx, ecx
        je short NoMatch // NULL pointer for pszMain
        test edx, edx
        je short NoMatch // NULL pointer for pszSub

        sub eax, eax
        mov al, [edi]
        push eax
        call DWORD PTR CharLower
        mov lowerch, al
        push eax
        call DWORD PTR CharUpper
        mov upperch, al

        push edi    // increment the second string pointer
        call DWORD PTR CharNext
        mov  edi, eax

        mov pszTmp2, edi
        mov edi, DWORD PTR CharNext // faster to call through a register

Loop1:
        mov al, [esi]
        test al, al
        je short NoMatch        // end of main string, so no match
        cmp al, lowerch
        je short CheckString    // lowercase match?
        cmp al, upperch
        je short CheckString    // upppercase match?
        push esi
        call edi                // Call CharNext to update main string pointer
        mov esi, eax
        jmp short Loop1

CheckString:
        mov pszTmp1, esi    // save current pszMain pointer in case its a match
        push esi
        call edi            // first character of both strings match,
        mov  esi, eax       // so move to next pszMain character
        mov edi, pszTmp2
        mov al, [edi]
        jmp short Branch1

Loop3:
        push esi
        call DWORD PTR CharNext    // CharNext to change pszMain pointer
        mov  esi, eax
        push edi
        call DWORD PTR CharNext    // CharNext to change pszSub pointer
        mov  edi, eax

        mov al, [edi]
Branch1:
        test al, al
        je short Match       // zero in sub string, means we've got a match
        cmp al, [esi]
        je short Loop3

        // Doesn't match, but might be simply a case mismatch. Lower-case both
        // characters and compare again

        sub ecx, ecx
        mov cl, al  // character from pszSub
        push ecx
        call DWORD PTR CharLower
        mov cl, al
        sub eax, eax
        mov al,  [esi]   // character from pszMain
        push ecx    // preserve register
        push eax
        call DWORD PTR CharLower
        pop ecx
        cmp al, cl
        je short Loop3  // we still have a match, keep checking

        // No match, put everything back, update pszMain to the next character
        // and try again from the top

        mov esi, pszTmp1
        mov  edi, DWORD PTR CharNext
        push esi
        call edi
        mov  esi, eax
        jmp short Loop1

Match:
        mov eax, pszTmp1
        jmp short Done  // Don't just return -- always let the C portion of the code handle the return

NoMatch:
        sub eax, eax
Done:
     }

    // Note lack of return in the C portion of the code. Return value is always in
    // eax register which we have set by the time we get here
}

WCHAR* __fastcall stristrW(const WCHAR* pszMain, const WCHAR* pszSub)
{
    pszMain;    // compiler thinks these are unreferenced
    pszSub;

    WCHAR* pszTmp1;
    WCHAR* pszTmp2;
    WCHAR  lowerch, upperch;

// We keep the first character of pszSub in lowerch and upperch (lower and
// upper case). First we loop trying to find a match for this character. Once
// we have found a match, we start with the second character of both pszMain
// and pszSub and walk through both strings doing a CharLower on both
// characters before comparing. If we make it all the way through pszSub with
// matches, then we bail with a pointer to the strings location in pszMain.

    _asm {
        mov esi, ecx    // pszMain
        mov edi, edx    // pszSub

        // Check for NULL pointers

        test ecx, ecx
        je short NoMatch // NULL pointer for pszMain
        test edx, edx
        je short NoMatch // NULL pointer for pszSub

        sub eax, eax
        mov ax, [edi]
        push eax
        call DWORD PTR CharLowerW
        mov lowerch, ax
        push eax
        call DWORD PTR CharUpperW
        mov upperch, ax

        lea edi, [edi+2]

        mov pszTmp2, edi

Loop1:
        mov ax, [esi]
        test ax, ax
        je short NoMatch        // end of main string, so no match
        cmp ax, lowerch
        je short CheckString    // lowercase match?
        cmp ax, upperch
        je short CheckString    // upppercase match?
        lea esi, [esi+2]
        jmp short Loop1

CheckString:
        mov pszTmp1, esi    // save current pszMain pointer
        lea esi, [esi+2]
        mov edi, pszTmp2
        mov ax, [edi]
        jmp short Branch1

Loop3:
        lea esi, [esi+2]
        lea edi, [edi+2]

        mov ax, [edi]
Branch1:
        test ax, ax
        je short Match       // zero in main string, means we've got a match
        cmp ax, [esi]
        je short Loop3

        // Doesn't match, but might be simply a case mismatch. Lower-case both
        // characters and compare again

        sub ecx, ecx
        mov cx, ax  // character from pszSub
        push ecx
        call DWORD PTR CharLowerW
        mov cx, ax
        sub eax, eax
        mov ax, [esi]   // character from pszMain
        push ecx        // preserve register
        push eax
        call DWORD PTR CharLowerW
        pop ecx
        cmp ax, cx
        je short Loop3  // we still have a match, keep checking

        // No match, put everything back, update pszMain to the next character
        // and try again from the top

        mov esi, pszTmp1
        lea esi, [esi+2]
        jmp short Loop1

Match:
        mov eax, pszTmp1
        jmp short Done

NoMatch:
        sub eax, eax
Done:
     }
    // Note lack of return in the C portion of the code. Return value is always in
    // eax register which we have set by the time we get here
}
